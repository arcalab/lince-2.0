package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.backend.Semantics.St
import lince.syntax.{Lince, Show}
import lince.syntax.Lince.*
import Program.*

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
    if st._3<=0 || st._4<=0 then
      return Set()
    given v:Valuation = st._2
    val t=st._3
    val lp=st._4

    st._1 match {
      case Skip => Set()
      case Assign(n, e) =>
        Set(s"$n:=${Show(e)}" -> (Skip, v+(n->Eval(e)),t,lp))
      case Seq(Skip, q) => next(q,v,t,lp)
      case Seq(p, q) =>
        for (a,(p2,v2,t2,lp2)) <- next((p,v,t,lp))
          yield a -> (Seq(p2,q),v2,t2,lp2)
      case ITE(b, pt, pf) =>
        if Eval(b) then Set("if-true"  -> (pt,v,t,lp))
                   else Set("if-false" -> (pf,v,t,lp))
      case wh@While(b, p) =>
        if Eval(b) then Set("wh-true"  -> (Seq(p,wh),v,t,lp-1))
                   else Set("wh-false" -> (Skip,v,t,lp))
      case EqDiff(eqs, durExp) =>
        val dur = Eval(durExp)
        if dur>t
        then Set("diff-stop" -> (Skip,RungeKutta(v,eqs,t),0,lp))
        else Set("diff-skip" -> (Skip,RungeKutta(v,eqs,dur),t-dur,lp))
    }

