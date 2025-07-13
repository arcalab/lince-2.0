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

  case class St(p: Program   // input program
               ,v: Valuation // known variables
               ,r: Random    // random generator
               ,t: Double    // maximum time
               ,lp:Int):      // maximum loops
    def empty = St(Program.Skip, Map(), new Random, 0,0)

  override def accepting(s: St): Boolean =
    s.t<=0 || s.lp<=0

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:Action](st: St): Set[(A, St)] =
    step(st).toSet

  /** Performs a single (deterministic) small step */
  def step(st: St): Option[(Action, St)] =
    if st.t<=0 || st.lp<=0 then
      return None
    given v:Valuation = st.v
    given r:Random = st.r

    st.p match {
      case Skip => None
      case Assign(n, e) =>
        val res = Eval(e)
        Some(Action.Assign(n,res) ->  st.copy(p = Skip, v = v+(n->res)))
      case Seq(Skip, q) => step(st.copy(p=q))
      case Seq(p, q) =>
        for (a,st2) <- step(st.copy(p=p))
          yield a -> st2.copy(p=Seq(st2.p,q))
      case ITE(b, pt, pf) =>
        if Eval(b) then Some(Action.CheckIf(b,true)  -> st.copy(p=pt))
                   else Some(Action.CheckIf(b,false) -> st.copy(p=pf))
      case wh@While(b, p) =>
        if Eval(b) then Some(Action.CheckWhile(b,true)  -> st.copy(p=Seq(p,wh), lp=st.lp-1))
                   else Some(Action.CheckWhile(b,false) -> st.copy(p=Skip))
      case EqDiff(eqs, durExp) =>
        val dur = Eval(durExp)
        val eqs2 = eqs.map(kv => (kv._1,Eval.rands(kv._2)))
        if dur>st.t
        then Some(Action.DiffStop(eqs2,st.t) ->
                  st.copy(p=EqDiff(eqs2,Expr.Num(dur-st.t)), v=RungeKutta(v,eqs2,st.t), t=0))
        else Some(Action.DiffSkip(eqs2,dur) ->
                  st.copy(p=Skip, v=RungeKutta(v,eqs2,dur), t=st.t-dur))
    }

