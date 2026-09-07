package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.backend.Stream
import Stream.Streams
import lince.syntax.Lince.*
import Program.*

/**
 * Concurrent small-step semantics.
 *
 * - coordinates multiple named programs
 * - schedules instantaneous steps deterministically
 * - evolves active differential equations simultaneously
 * - delegates ordinary program steps to BasicSmallStep
 */
object ConcurrentSmallStep
    extends SOS[Action, ConcurrentSmallStep.ConcurrentState]:

  case class ConcurrentState(
      progs: Map[String, Program],
      v: Valuation,
      s: Streams,
      t: Double,
      lp: Int,
      nextProcess: Int = 0
  )

  val defaultRKSamples = 100

  override def accepting(
      s: ConcurrentState
  ): Boolean =
    s.t <= 0 || s.lp <= 0

  def next[A >: Action](
      st: ConcurrentState
  ): Set[(A, ConcurrentState)] =
    step(st)(using defaultRKSamples).toSet

  /**
   * Deterministic round-robin selection of the next
   * instantaneous component.
   *
   * Differential equations are not selected here because
   * they are handled together by stepContinuous.
   */
  private def scheduledProgram(
      st: ConcurrentState
  ): Option[(Int, String, Program)] =

    val names = processOrder(st)

    if names.isEmpty then
      None
    else
      val size = names.size
      val start =
        Math.floorMod(st.nextProcess, size)

      (0 until size).iterator
        .map(offset =>
          (start + offset) % size
        )
        .flatMap { index =>
          val name = names(index)

          st.progs
            .get(name)
            .map { prog =>
              (index, name, prog)
            }
        }
        .find { case (_, _, prog) =>
          nextWithRest(prog)._1 match
            case Skip =>
              false

            case EqDiff(_, _) =>
              false

            case _ =>
              true
        }

  def step(
      st: ConcurrentState
  )(using rkSamples: Int)
      : Option[(Action, ConcurrentState)] =

    if st.t <= 0 || st.lp <= 0 then
      return None

    scheduledProgram(st) match
      case Some(_) =>
        stepOne(st)

      case None =>
        stepContinuous(st)

  /**
   * A currently active continuous evolution.
   *
   * dur = Some(d) means a finite remaining duration.
   * dur = None means an unbounded evolution.
   */
  private case class Flow(
      name: String,
      eqs: Map[Location, Expr],
      dur: Option[Double],
      rest: Program
  )

  private def stepContinuous(
      st: ConcurrentState
  )(using rkSamples: Int)
      : Option[(Action, ConcurrentState)] =

    given v0: Valuation = st.v

    var ss = st.s
    var failed = false

    val flows: List[Flow] =
      processOrder(st).flatMap { name =>

        st.progs.get(name).flatMap { p =>

          nextWithRest(p) match

            case (EqDiff(eqs, durExp), rest) =>

              val eqs2 =
                for (x, e) <- eqs yield
                  Eval.evalStreams(e, ss) match

                    case None =>
                      failed = true
                      x -> e

                    case Some((e2, ss2)) =>
                      ss = ss2
                      x -> e2

              val dur2: Option[Double] =
                durExp match

                  case None =>
                    None

                  case Some(d) =>
                    Eval.evalStreams(d, ss) match

                      case None =>
                        failed = true
                        None

                      case Some((d2, ss2)) =>
                        ss = ss2
                        Some(Eval.asDouble(d2))

              Some(
                Flow(
                  name,
                  eqs2,
                  dur2,
                  rest
                )
              )

            case _ =>
              None
        }
      }

    if failed || flows.isEmpty then
      None

    else

      /*
       * Only finite flows contribute to the next
       * continuous boundary.
       */
      val finiteDurations =
        flows.flatMap(_.dur)

      val minDur: Option[Double] =
        finiteDurations.minOption

      /*
       * Evolve either until:
       *
       *   - the first finite flow finishes, or
       *   - the global simulation time expires.
       *
       * If every flow is infinite, evolve until st.t.
       */
      val realDur =
        minDur match
          case Some(d) =>
            d.min(st.t)

          case None =>
            st.t

      val mergedEqs: Map[Location, Expr] =
        flows
          .flatMap(_.eqs)
          .toMap

      val v2 =
        RungeKutta(
          st.v,
          mergedEqs,
          realDur,
          rkSamples
        )

      val newProgs =
        st.progs.map {
          case (name, oldProg) =>

            flows.find(_.name == name) match

              case None =>
                name -> oldProg

              case Some(flow) =>

                flow.dur match

                  /*
                   * Infinite ODE:
                   * it remains active after this
                   * continuous interval.
                   */
                  case None =>
                    name ->
                      mkSeq(
                        EqDiff(
                          flow.eqs,
                          None
                        ) ::
                          flattenSeq(flow.rest)
                      )

                  /*
                   * Finite ODE.
                   */
                  case Some(dur) =>
                    val remaining =
                      dur - realDur

                    if remaining <= 0 then
                      name -> flow.rest

                    else
                      name ->
                        mkSeq(
                          EqDiff(
                            flow.eqs,
                            Some(
                              Expr.Num(remaining)
                            )
                          ) ::
                            flattenSeq(flow.rest)
                        )
        }

      val st2 =
        st.copy(
          progs = newProgs,
          v = v2,
          s = ss,
          t = st.t - realDur
        )

      val action =
        minDur match

          /*
           * No finite ODE exists, so the only thing
           * stopping us is the global time bound.
           */
          case None =>
            Action.DiffStop(
              mergedEqs,
              realDur
            )

          case Some(d) if d > st.t =>
            Action.DiffStop(
              mergedEqs,
              realDur
            )

          case Some(_) =>
            Action.DiffSkip(
              mergedEqs,
              realDur
            )

      Some(
        action -> st2
      )

  def stepOne(
      st: ConcurrentState
  )(using rkSamples: Int)
      : Option[(Action, ConcurrentState)] =

    scheduledProgram(st) match

      case Some((index, name, prog)) =>

        val names =
          processOrder(st)

        val nextIndex =
          if names.isEmpty then
            0
          else
            (index + 1) % names.size

        stepProgram(
          name,
          prog,
          st
        )(using rkSamples)
          .map { case (action, st2) =>
            action ->
              st2.copy(
                nextProcess = nextIndex
              )
          }

      case None =>
        None

  /**
   * Delegate one component's ordinary small step to
   * the sequential semantics.
   */
  def stepProgram(
      name: String,
      prog: Program,
      st: ConcurrentState
  )(using rkSamples: Int)
      : Option[(Action, ConcurrentState)] =

    prog match

      case Skip =>
        None

      case _ =>

        val basic =
          BasicSmallStep.BasicState(
            prog,
            st.v,
            st.s,
            st.t,
            st.lp
          )

        BasicSmallStep
          .step(basic)(using rkSamples)
          .map {
            case (a, basic2) =>

              val updated =
                st.copy(
                  progs =
                    st.progs.updated(
                      name,
                      basic2.p
                    ),
                  v = basic2.v,
                  s = basic2.s,
                  t = basic2.t,
                  lp = basic2.lp
                )

              a -> updated
          }

  private def flattenSeq(
      p: Program
  ): List[Program] =

    p match
      case Skip =>
        Nil

      case Seq(p, q) =>
        flattenSeq(p) ++
          flattenSeq(q)

      case _ =>
        List(p)

  private def mkSeq(
      ps: List[Program]
  ): Program =

    ps match
      case Nil =>
        Skip

      case h :: Nil =>
        h

      case h :: t =>
        Seq(
          h,
          mkSeq(t)
        )

  private def nextWithRest(
      p: Program
  ): (Program, Program) =

    flattenSeq(p) match
      case Nil =>
        Skip -> Skip

      case h :: t =>
        h -> mkSeq(t)

  private def processOrder(
      st: ConcurrentState
  ): List[String] =

    st.progs.keys.toList.sortBy {
      name =>
        if name == "" then " "
        else name
    }