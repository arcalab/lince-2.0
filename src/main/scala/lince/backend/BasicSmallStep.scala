package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.syntax.Lince.*
import Program.*

import scala.util.Random

/**
 * Basic small-step semantics for a single Lince program.
 *
 * State shape:
 *   - p: current program
 *   - v: valuation
 *   - s: random seed
 *   - t: remaining time
 *   - lp: remaining loop unfoldings
 *
 * This is the base semantics reused by ConcurrentSmallStep.
 */

object BasicSmallStep extends SOS[Action, BasicSmallStep.BasicState]:

  case class BasicState(
      p: Program,
      v: Valuation,
      s: Long,
      t: Double,
      lp: Int
  ):
    def nextSeed: BasicState =
      resetSeed
      this.copy(s = rand.nextLong())

    def resetSeed: Unit =
      rand.setSeed(s)

  val rand: Random = new Random
  val defaultRKSamples = 100

  override def accepting(s: BasicState): Boolean =
    s.t <= 0 || s.lp <= 0

  def next[A >: Action](st: BasicState): Set[(A, BasicState)] =
    step(st)(using defaultRKSamples).toSet

  def step(st: BasicState)(using rkSamples: Int): Option[(Action, BasicState)] =
    if st.t <= 0 || st.lp <= 0 then
      return None

    st.resetSeed

    given r: Random = rand
    given v: Valuation = st.v

    st.p match
      case Skip =>
        None
      case Assign(n, e) =>
        val res = Eval(e)
        Some(
          Action.Assign(n, res) ->
            st.nextSeed.copy(
              p = Skip,
              v = v + (n -> res)
            )
        )
      case Seq(Skip, q) =>
        step(st.copy(p = q))
      case Seq(p, q) =>
        for (a, st2) <- step(st.copy(p = p))
        yield a -> st2.copy(p = Seq(st2.p, q))
      case ITE(b, pt, pf) =>
        if Eval(b) then
          Some(Action.CheckIf(b, true) ->
            st.nextSeed.copy(p = pt))
        else
          Some(Action.CheckIf(b, false) ->
            st.nextSeed.copy(p = pf))
      case wh @ While(b, p) =>
        if Eval(b) then
          Some(Action.CheckWhile(b, true) ->
            st.nextSeed.copy(
              p = Seq(p, wh),
              lp = st.lp - 1
            ))
        else
          Some(Action.CheckWhile(b, false) ->
            st.nextSeed.copy(p = Skip))
      case EqDiff(eqs, durExp) =>
        val dur = Eval(durExp)
        val eqs2 = eqs.map(kv => (kv._1, Eval.rands(kv._2)))
        if dur > st.t then
          val v2 = RungeKutta(v, eqs2, st.t, rkSamples)
          Some(
            Action.DiffStop(eqs2, st.t) ->
              st.nextSeed.copy(
                p = EqDiff(eqs2, Expr.Num(dur - st.t)),
                v = v2,
                t = 0
              )
          )
        else
          val v2 = RungeKutta(v, eqs2, dur, rkSamples)
          Some(
            Action.DiffSkip(eqs2, dur) ->
              st.nextSeed.copy(
                p = Skip,
                v = v2,
                t = st.t - dur
              )
          )