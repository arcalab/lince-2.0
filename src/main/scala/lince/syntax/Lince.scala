package lince.syntax

/**
 * Internal structure to represent terms in Lince 2.0.
 */

object Lince:

  enum Program:
    case Skip
    case Assign(v:String, e:Expr)
    case EqDiff(eqs:Map[String,Expr], dur:Expr)
    case Seq(p:Program, q:Program)
    case ITE(b:Cond, pt:Program, pf:Program)
    case While(b:Cond, p:Program)

  enum Cond:
    case True
    case False
    case Comp(op:String, e1:Expr, e2:Expr)
    case And(c1: Cond, c2: Cond)
    case Or(c1: Cond, c2: Cond)
    case Not(c: Cond)

  enum Expr:
    case Num(n:Double)
    case Var(x:String)
    case Func(op:String, es:List[Expr])




