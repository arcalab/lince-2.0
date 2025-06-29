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

  type St = (Program   // input program
            ,Valuation // known variables
            ,Double    // maximum time
            ,Int)      // maximum loops

  override def accepting(s: St): Boolean =
    s._3<=0 || s._4<=0

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:String](st: St): Set[(A, St)] =
    step(st).toSet

  /** Performs a single (deterministic) small step */
  def step(st: St): Option[(String, St)] =
    if st._3<=0 || st._4<=0 then
      return None
    given v:Valuation = st._2
    val t=st._3
    val lp=st._4

    st._1 match {
      case Skip => None
      case Assign(n, e) =>
        Some(s"$n:=${Show(e)}" -> (Skip, v+(n->Eval(e)),t,lp))
      case Seq(Skip, q) => step(q,v,t,lp)
      case Seq(p, q) =>
        for (a,(p2,v2,t2,lp2)) <- step((p,v,t,lp))
          yield a -> (Seq(p2,q),v2,t2,lp2)
      case ITE(b, pt, pf) =>
        if Eval(b) then Some("if-true"  -> (pt,v,t,lp))
                   else Some("if-false" -> (pf,v,t,lp))
      case wh@While(b, p) =>
        if Eval(b) then Some(s"wh-true (${Show(b)})"  -> (Seq(p,wh),v,t,lp-1))
                   else Some(s"wh-false (${Show(b)})" -> (Skip,v,t,lp))
      case EqDiff(eqs, durExp) =>
        val dur = Eval(durExp)
        if dur>t
        then Some("diff-stop" -> (EqDiff(eqs,Expr.Num(dur-t)),RungeKutta(v,eqs,t),0,lp))
        else Some("diff-skip" -> (Skip,RungeKutta(v,eqs,dur),t-dur,lp))
    }

  @tailrec
  def nextDisc[A>:String](st: St, hist:List[String]=Nil): (List[String],St) =
    step(st) match
      case None => hist -> st
      case Some((a,st2)) =>
        if st._3 == st2._3 then nextDisc(st2,a::hist) else (hist,st) //(a::hist) -> st2
