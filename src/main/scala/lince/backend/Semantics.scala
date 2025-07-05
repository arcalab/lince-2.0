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

    st._1 match {
      case Skip => None
      case Assign(n, e) =>
        Some(s"$n:=${Show(e)}" -> St(Skip, v+(n->Eval(e)),t,lp))
      case Seq(Skip, q) => step(St(q,v,t,lp))
      case Seq(p, q) =>
        for (a,St(p2,v2,t2,lp2)) <- step(St(p,v,t,lp))
          yield a -> St(Seq(p2,q),v2,t2,lp2)
      case ITE(b, pt, pf) =>
        if Eval(b) then Some(s"if-true: ${Show(b)}"  -> St(pt,v,t,lp))
                   else Some(s"if-false: ${Show(b)}" -> St(pf,v,t,lp))
      case wh@While(b, p) =>
        if Eval(b) then Some(s"wh-true: ${Show(b)}"  -> St(Seq(p,wh),v,t,lp-1))
                   else Some(s"wh-false: ${Show(b)}" -> St(Skip,v,t,lp))
      case EqDiff(eqs, durExp) =>
        val dur = Eval(durExp)
        if dur>t
        then Some("diff-stop" -> St(EqDiff(eqs,Expr.Num(dur-t)),RungeKutta(v,eqs,t),0,lp))
        else Some("diff-skip" -> St(Skip,RungeKutta(v,eqs,dur),t-dur,lp))
    }

