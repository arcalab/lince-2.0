package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.backend.Stream
import Stream.Streams
import lince.syntax.{Lince, Show}
import lince.syntax.Lince.*
import Program.*

/**
 * Basic small-step semantics for a single Lince program.
 *
 * This contains the sequential semantics from Lince and is reused
 * by ConcurrentSmallStep for instantaneous component steps.
 */
object BasicSmallStep
    extends SOS[Action, BasicSmallStep.BasicState]:

  case class BasicState(
      p: Program,
      v: Valuation,
      s: Streams,
      t: Double,
      lp: Int
  )

  val defaultRKSamples = 100

  override def accepting(s: BasicState): Boolean =
    s.t <= 0 || s.lp <= 0

  def next[A >: Action](
      st: BasicState
  ): Set[(A, BasicState)] =
    step(st)(using defaultRKSamples).toSet

  def step(
      st: BasicState
  )(using rkSamples: Int): Option[(Action, BasicState)] =

    if st.t <= 0 || st.lp <= 0 then
      return None

    given v: Valuation = st.v

    st.p match

      case Skip =>
        None

      case Assign(n, e) =>
        if n.prog.isEmpty && st.s.contains(n.name) then
          sys.error(
            s"Variable definition ${Show(st.p)} overriding an existing stream."
          )

        val ress = Eval.asDouble(e, st.s)

        ress.map { case (res, ss) =>
          Action.Assign(n, res) ->
            st.copy(
              p = Skip,
              v = v + (n -> res),
              s = ss
            )
        }

      case StreamDef(n, stream) =>
        if st.v.contains(Location(None, n)) then
          sys.error(
            s"Stream definition ${Show(st.p)} overriding an existing variable."
          )

        st.s.get(n) match
          case Some(strm) if strm.keep =>
            Some(
              Action.StrmDef(n, stream) ->
                st.copy(p = Skip)
            )

          case _ =>
            Some(
              Action.StrmDef(n, stream) ->
                st.copy(
                  p = Skip,
                  s = st.s + (n -> stream)
                )
            )

      case Seq(Skip, q) =>
        step(st.copy(p = q))

      case Seq(p, q) =>
        for
          (a, st2) <- step(st.copy(p = p))
        yield
          a -> st2.copy(
            p = Seq(st2.p, q)
          )

      case ITE(b, pt, pf) =>
        Eval.asBoolean(b, st.s) match
          case Some((true, ss)) =>
            Some(
              Action.CheckIf(b, true) ->
                st.copy(
                  p = pt,
                  s = ss
                )
            )

          case Some((false, ss)) =>
            Some(
              Action.CheckIf(b, false) ->
                st.copy(
                  p = pf,
                  s = ss
                )
            )

          case None =>
            None

      case wh @ While(b, p) =>
        Eval.asBoolean(b, st.s) match
          case Some((true, ss)) =>
            Some(
              Action.CheckWhile(b, true) ->
                st.copy(
                  p = Seq(p, wh),
                  lp = st.lp - 1,
                  s = ss
                )
            )

          case Some((false, ss)) =>
            Some(
              Action.CheckWhile(b, false) ->
                st.copy(
                  p = Skip,
                  s = ss
                )
            )

          case None =>
            None

      case EqDiff(eqs, durExp) =>

        var ss = st.s
        var stop = false

        val eqs2 =
          for (x, e) <- eqs yield
            Eval.evalStreams(e, ss) match
              case None =>
                stop = true
                x -> e

              case Some((e2, ss2)) =>
                ss = ss2
                x -> e2

        val durExp2 =
          durExp.map { d =>
            Eval.evalStreams(d, ss) match
              case None =>
                stop = true
                d

              case Some((d2, ss2)) =>
                ss = ss2
                d2
          }

        if stop then
          None
        else
          durExp2.map(Eval.asDouble) match

            case Some(dur) if dur > st.t =>
              val v2 =
                RungeKutta(
                  v,
                  eqs2,
                  st.t,
                  rkSamples
                )

              Some(
                Action.DiffStop(eqs2, st.t) ->
                  st.copy(
                    p = EqDiff(
                      eqs2,
                      Some(Expr.Num(dur - st.t))
                    ),
                    v = v2,
                    t = 0,
                    s = ss
                  )
              )

            case Some(dur) =>
              val v2 =
                RungeKutta(
                  v,
                  eqs2,
                  dur,
                  rkSamples
                )

              Some(
                Action.DiffSkip(eqs2, dur) ->
                  st.copy(
                    p = Skip,
                    v = v2,
                    t = st.t - dur,
                    s = ss
                  )
              )

            case None =>
              val v2 =
                RungeKutta(
                  v,
                  eqs2,
                  st.t,
                  rkSamples
                )

              Some(
                Action.DiffStop(eqs2, st.t) ->
                  st.copy(
                    p = EqDiff(eqs2, None),
                    v = v2,
                    t = 0,
                    s = ss
                  )
              )