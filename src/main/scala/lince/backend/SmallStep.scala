package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.backend.SmallStep.St
import lince.syntax.{Lince, Show}
import lince.syntax.Lince.*
import Program.*

import scala.annotation.tailrec
import scala.util.Random

/** Small-step semantics for both commands and boolean+integer expressions.  */
object SmallStep extends SOS[Action,St]:

  case class St(progs: Map[String, Program]   // input program
               ,v: Valuation // known variables
//               ,r: Random    // random generator
               ,s: Long      // seed for the random generator
               ,t: Double    // maximum time
               ,lp:Int):     // maximum loops
    /** Creates a new state with an updated random seed */
    def nextSeed: St =
      resetSeed
      this.copy(s = rand.nextLong())
    def resetSeed: Unit =
      rand.setSeed(s)

  val rand: Random = new Random
  val defaultRKSamples = 100

  override def accepting(s: St): Boolean =
    s.t<=0 || s.lp<=0

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:Action](st: St): Set[(A, St)] =
    step(st)(using defaultRKSamples).toSet

  // Collect all differential equations if ALL programs are EqDiff
  def collectFlows(progs: Map[String, Program])(using v0: Valuation, r0: Random): Option[(Map[Location, Expr], Double)] =
    val diffs = progs.collect {
      case (_, EqDiff(eqs, dur)) => (eqs, dur)
    }
    if diffs.size != progs.size then None
    else
      val duration = Eval(diffs.head._2)(using v0, r0)
      val merged: Map[Location, Expr] =
        diffs.flatMap { case (eqs, _) => eqs }.toMap
      Some((merged, duration))

  /** Performs a single (deterministic) small step */
  def step(st: St)(using rkSamples: Int): Option[(Action, St)] =
    if st.t <= 0 || st.lp <= 0 then
      return None
    st.resetSeed
    given r0: Random = rand
    given v0: Valuation = st.v
    // Try PARALLEL continuous evolution first
    collectFlows(st.progs) match
      case Some((eqs, dur)) =>
        val eqs2 = eqs.map((k, e) => (k, Eval.rands(e)(using v0, r0)))
        if dur > st.t then
          val v2 = RungeKutta(st.v, eqs2, st.t, rkSamples)
          val newProgs = st.progs.map {
            case (name, EqDiff(eqsP, _)) =>
              name -> EqDiff(eqsP, Expr.Num(dur - st.t))
            case other => other
          }
          Some(
            Action.DiffStop(eqs2, st.t) ->
              st.nextSeed.copy(
                progs = newProgs,
                v = v2,
                t = 0
              )
          )
        else
          val v2 = RungeKutta(st.v, eqs2, dur, rkSamples)
          val newProgs = st.progs.map {
            case (name, EqDiff(_, _)) => name -> Skip
            case other                => other
          }
          Some(
            Action.DiffSkip(eqs2, dur) ->
              st.nextSeed.copy(
                progs = newProgs,
                v = v2,
                t = st.t - dur
              )
          )
      // Otherwise fallback to sequential execution
      case None =>
        stepOne(st)(using rkSamples)

  def stepOne(st: St)(using rkSamples: Int): Option[(Action, St)] =
    val (name, prog) = st.progs.head
    stepProgram(name, prog, st)(using rand, st.v, rkSamples)

  def stepProgram(name: String, prog: Program, st: St)
  (using r0: Random, v0: Valuation, rkSamples: Int): Option[(Action, St)] =

  prog match {

    case Skip => None

    case Assign(loc, e) =>
      val res = Eval(e)(using v0, r0)
      Some(Action.Assign(loc, res) ->
        st.nextSeed.copy(
          progs = st.progs.updated(name, Skip),
          v = st.v + (loc -> res)
        ))

    case Seq(Skip, q) =>
      stepProgram(name, q, st.copy(progs = st.progs.updated(name, q)))(using r0, v0, rkSamples)

    case Seq(p, q) =>
      for (a, st2) <- stepProgram(name, p, st.copy(progs = st.progs.updated(name, p)))(using r0, v0, rkSamples)
      yield a -> st2.copy(
        progs = st2.progs.updated(name, Seq(st2.progs(name), q))
      )

    case ITE(b, pt, pf) =>
      if Eval(b)(using v0, r0) then
        Some(Action.CheckIf(b,true) ->
          st.nextSeed.copy(progs = st.progs.updated(name, pt)))
      else
        Some(Action.CheckIf(b,false) ->
          st.nextSeed.copy(progs = st.progs.updated(name, pf)))

    case wh @ While(b, p) =>
      if Eval(b)(using v0, r0) then
        Some(Action.CheckWhile(b,true) ->
          st.nextSeed.copy(
            progs = st.progs.updated(name, Seq(p, wh)),
            lp = st.lp - 1
          ))
      else
        Some(Action.CheckWhile(b,false) ->
          st.nextSeed.copy(progs = st.progs.updated(name, Skip)))

    case EqDiff(eqs, durExp) =>
        val dur = Eval(durExp)(using v0, r0)
        val eqs2 = eqs.map(kv => (kv._1, Eval.rands(kv._2)(using v0, r0)))
        if dur > st.t then
          val v2 = RungeKutta(st.v, eqs2, st.t,rkSamples)
          Some(
            Action.DiffStop(eqs2, st.t) ->
              st.nextSeed.copy(
                progs = st.progs.updated(name, EqDiff(eqs2, Expr.Num(dur - st.t))),
                v = v2,
                t = 0
              )
          )
        else
          val v2 = RungeKutta(st.v, eqs2, dur,rkSamples)
          Some(
            Action.DiffSkip(eqs2, dur) ->
              st.nextSeed.copy(
                progs = st.progs.updated(name, Skip),
                v = v2,
                t = st.t - dur
              )
          )
  }

