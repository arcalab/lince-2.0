package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.backend.Semantics.St
import lince.syntax.{Lince, Show}
import lince.syntax.Lince.*
import Program.*

import scala.annotation.tailrec

/** Small-step semantics for both commands and boolean+integer expressions.  */
object Semantics extends SOS[String,St]:

  case class St(p: Program   // input program
               ,v: Valuation // known variables
               ,t: Double    // maximum time
               ,lp:Int)      // maximum loops

  override def accepting(s: St): Boolean =
    s.t<=0 || s.lp<=0

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:String](st: St): Set[(A, St)] =
    step(st).toSet

  /** Performs a single (deterministic) small step */
  def step(st: St): Option[(String, St)] =
    if st.t<=0 || st.lp<=0 then
      return None
    given v:Valuation = st.v
    val t = st.t
    val lp = st.lp

    st.p match {
      case Skip => None
      case Assign(n, e) =>
        Some(s"$n:=${Show(e)}" ->  st.copy(p = Skip, v = v+(n->Eval(e))))
      case Seq(Skip, q) => step(st.copy(p=q))
      case Seq(p, q) =>
        for (a,st2) <- step(st.copy(p=p))
          yield a -> st2.copy(p=Seq(st2.p,q))
      case ITE(b, pt, pf) =>
        if Eval(b) then Some(s"if-true: ${Show(b)}"  -> st.copy(p=pt))
                   else Some(s"if-false: ${Show(b)}" -> st.copy(p=pf))
      case wh@While(b, p) =>
        if Eval(b) then Some(s"wh-true: ${Show(b)}"  -> st.copy(p=Seq(p,wh), lp=lp-1))
                   else Some(s"wh-false: ${Show(b)}" -> st.copy(p=Skip))
      case EqDiff(eqs, durExp) =>
        val dur = Eval(durExp)
        if dur>t
        then Some("diff-stop" -> st.copy(p=EqDiff(eqs,Expr.Num(dur-t)), v=RungeKutta(v,eqs,t), t=0))
        else Some("diff-skip" -> st.copy(p=Skip, v=RungeKutta(v,eqs,dur), t=t-dur))
    }

