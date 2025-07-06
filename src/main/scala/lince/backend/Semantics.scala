package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.backend.Semantics.St
import lince.syntax.{Lince, Show}
import lince.syntax.Lince.*
import Program.*

import scala.annotation.tailrec

/** Small-step semantics for both commands and boolean+integer expressions.  */
object Semantics extends SOS[Action,St]:

  case class St(p: Program   // input program
               ,v: Valuation // known variables
               ,t: Double    // maximum time
               ,lp:Int)      // maximum loops

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
        if dur>st.t
        then Some(Action.DiffStop(eqs,st.t) ->
                  st.copy(p=EqDiff(eqs,Expr.Num(dur-st.t)), v=RungeKutta(v,eqs,st.t), t=0))
        else Some(Action.DiffSkip(eqs,dur) ->
                  st.copy(p=Skip, v=RungeKutta(v,eqs,dur), t=st.t-dur))
    }

