package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.syntax.Lince.*
import Program.*

import scala.util.Random

/**
 * Concurrent small-step semantics.
 *
 * Responsibilities:
 *   - coordinates multiple named programs
 *   - performs parallel continuous evolution when all programs are EqDiff
 *   - delegates ordinary single-program steps to BasicSmallStep
 *
 * Scheduling is currently deterministic: when no parallel flow is possible,
 * the first program in the map is stepped.
 */

object ConcurrentSmallStep extends SOS[Action, ConcurrentSmallStep.ConcurrentState]:

  case class ConcurrentState(
      progs: Map[String, Program],
      v: Valuation,
      s: Long,
      t: Double,
      lp: Int,
      nextProcess: Int = 0
  ):
    def nextSeed: ConcurrentState =
      resetSeed
      this.copy(s = rand.nextLong())

    def resetSeed: Unit =
      rand.setSeed(s)

  val rand: Random = new Random
  val defaultRKSamples = 100

  private def scheduledProgram(st: ConcurrentState): Option[(Int, String, Program)] =
    val names = processOrder(st)
    if names.isEmpty then None
    else
      val size = names.size
      val start = Math.floorMod(st.nextProcess, size)
      (0 until size).iterator
        .map(offset => (start + offset) % size)
        .flatMap { index =>
          val name = names(index)
          st.progs.get(name).map { prog =>
            (index, name, prog)
          }
        }
        .find { case (_, _, prog) =>
          nextWithRest(prog)._1 match
            case Skip         => false
            case EqDiff(_, _) => false
            case _            => true
        }

  override def accepting(s: ConcurrentState): Boolean =
    s.t <= 0 || s.lp <= 0

  def next[A >: Action](st: ConcurrentState): Set[(A, ConcurrentState)] =
    step(st)(using defaultRKSamples).toSet

  // Collect all differential equations if ALL programs are EqDiff
  def collectFlows(
      progs: Map[String, Program]
  )(using v0: Valuation, r0: Random)
      : Option[(Map[Location, Expr], Double)] =
    val diffs = progs.collect {
      case (_, EqDiff(eqs, dur)) => (eqs, dur)
    }
    if diffs.size != progs.size then None
    else
      val duration = Eval(diffs.head._2)(using v0, r0)
      val merged =
        diffs.flatMap { case (eqs, _) => eqs }.toMap
      Some((merged, duration))

  def step(
      st: ConcurrentState
  )(using rkSamples: Int): Option[(Action, ConcurrentState)] =

    if st.t <= 0 || st.lp <= 0 then
      return None

    st.resetSeed

    given r0: Random = rand
    given v0: Valuation = st.v

    scheduledProgram(st) match
      case Some(_) =>
        stepOne(st)

      case None =>
        stepContinuous(st)

  private case class Flow(
      name: String,
      eqs: Map[Location, Expr],
      dur: Double,
      rest: Program
  )

  private def stepContinuous(
      st: ConcurrentState
  )(using
      r0: Random,
      v0: Valuation,
      rkSamples: Int
  ): Option[(Action, ConcurrentState)] =

    val flows: List[Flow] =
      processOrder(st).flatMap { name =>
        st.progs.get(name).flatMap { p =>
          nextWithRest(p) match
            case (EqDiff(eqs, durExp), rest) =>
              val dur = Eval(durExp)(using v0, r0)
              val eqs2 = eqs.map { case (x, e) =>
                x -> Eval.rands(e)(using v0, r0)
              }
              Some(Flow(name, eqs2, dur, rest))

            case _ =>
              None
        }
      }

    if flows.isEmpty then
      None
    else
      val minDur: Double = flows.map(_.dur).min
      val realDur: Double = minDur.min(st.t)

      val mergedEqs: Map[Location, Expr] =
        flows.flatMap(_.eqs).toMap

      val v2 =
        RungeKutta(st.v, mergedEqs, realDur, rkSamples)

      val newProgs =
        st.progs.map { case (name, oldProg) =>
          flows.find(_.name == name) match
            case None =>
              name -> oldProg

            case Some(flow) =>
              val remaining = flow.dur - realDur

              if remaining <= 0 then
                name -> flow.rest
              else
                name -> mkSeq(
                  EqDiff(flow.eqs, Expr.Num(remaining)) ::
                    flattenSeq(flow.rest)
                )
        }

      val st2 =
        st.nextSeed.copy(
          progs = newProgs,
          v = v2,
          t = st.t - realDur
        )

      val action =
        if st.t <= minDur then
          Action.DiffStop(mergedEqs, realDur)
        else
          Action.DiffSkip(mergedEqs, realDur)

      Some(action -> st2)

  def stepOne(
      st: ConcurrentState
  )(using rkSamples: Int): Option[(Action, ConcurrentState)] =
    scheduledProgram(st) match
      case Some((index, name, prog)) =>
        val names = processOrder(st)
        val nextIndex =
          if names.isEmpty then 0
          else (index + 1) % names.size
        stepProgram(name, prog, st)(using rand, st.v, rkSamples)
          .map { case (action, st2) =>
            action -> st2.copy(nextProcess = nextIndex)
          }
      case None =>
        None

  def stepProgram(
      name: String,
      prog: Program,
      st: ConcurrentState
      )(using
      r0: Random,
      v0: Valuation,
      rkSamples: Int
  ): Option[(Action, ConcurrentState)] =

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

        BasicSmallStep.step(basic).map {
          case (a, basic2) =>
            val updated =
              st.copy(
                progs =
                  st.progs.updated(name, basic2.p),
                v = basic2.v,
                s = basic2.s,
                t = basic2.t,
                lp = basic2.lp
              )

            a -> updated
        }

  private def flattenSeq(p: Program): List[Program] = p match
    case Skip => Nil
    case Seq(p, q) => flattenSeq(p) ++ flattenSeq(q)
    case _ => List(p)

  private def mkSeq(ps: List[Program]): Program =
    ps match
      case Nil => Skip
      case h :: Nil => h
      case h :: t => Seq(h, mkSeq(t))

  private def nextWithRest(p: Program): (Program, Program) =
    flattenSeq(p) match
      case Nil => Skip -> Skip
      case h :: t => h -> mkSeq(t)

  private def isContinuous(p: Program): Boolean =
    nextWithRest(p)._1 match
      case EqDiff(_, _) => true
      case _ => false

  private def isFinished(p: Program): Boolean =
    nextWithRest(p)._1 == Skip

  private def processOrder(st: ConcurrentState): List[String] =
    st.progs.keys.toList.sortBy(name => if name == "" then " " else name)