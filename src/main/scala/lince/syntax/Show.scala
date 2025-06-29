package lince.syntax

import lince.syntax.Lince.*

/**
 * List of functions to produce textual representations of commands
 */
object Show:

  def apply(p: Program): String = p match
    case Program.Skip => "skip; "
    case Program.Assign(v, e) => s"$v:=${apply(e)}; "
    case Program.EqDiff(eqs, dur) =>
      eqs.map(kv => s"${kv._1}'=${apply(kv._2)}").mkString(", ") + s" for ${apply(dur)}; "
    case Program.Seq(p, q) => apply(p)+"\n"+apply(q)
    case Program.ITE(b, pt, Program.Skip) => s"if ${apply(b)}:\n${ind(apply(pt))}"
    case Program.ITE(b, pt, pf) => s"if ${apply(b)}:\n${ind(apply(pt))}\nelse\n${ind(apply(pf))}"
    case Program.While(b, p) => s"while ${apply(b)}:\n${ind(apply(p))}"

  def ind(s:String,i:Int=3) = (" "*i)+s.replaceAll("\n",s"\n${" "*i}")

  def apply(e: Expr): String = e match
    case Expr.Num(n) => n.toString
    case Expr.Var(x) => x
    case Expr.Func(op, es) if "+-/*^".contains(op.headOption.getOrElse(' ')) =>
      es.map(applyP).mkString(s"$op")
    case Expr.Func(op, es) =>
      s"$op(${es.map(apply).mkString(", ")})"

  def applyP(e: Expr): String = e match
    case Expr.Func(_,es) if es.size>1 => s"(${apply(e)})"
    case _ => apply(e)

  def apply(c: Cond): String = c match {
    case Cond.True => "true"
    case Cond.False => "false"
    case Cond.Comp(op, e1, e2) => s"${apply(e1)} $op ${apply(e2)}"
    case Cond.And(c1, c2) => s"${apply(c1)} && ${apply(c2)}"
    case Cond.Or(c1, c2) => s"${apply(c1)} || ${apply(c2)}"
    case Cond.Not(c) => s"!(${apply(c)})"
  }

  def simpleStatm(p: Program): String = p match {
    case Program.Seq(Program.Seq(p1,p2), p3) => simpleStatm(Program.Seq(p1,Program.Seq(p2,p3)))
    case Program.Seq(Program.Skip, p2) => "skip;"+simpleStatm(p2)
    case Program.Seq(p1, p2) => apply(p1)+"..."
    case _ => apply(p)
  }

  def simpleSt(st: lince.backend.Semantics.St): String =
    s"[${st._3}] ${st._2.mkString(",")}: ${simpleStatm(st._1)}"

