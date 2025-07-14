package lince.backend

import lince.syntax.Lince.{Cond, Expr}
import lince.syntax.{Lince, Show}

import scala.util.Random

object Eval:

  //  type Valuation = Map[String,Expr]
  type Valuation = Map[String,Double]
  private type MValuation = scala.collection.Map[String,Double]

  def apply(e:Expr)(using v:MValuation, rand:Random = new Random): Double = e match
    case Expr.Num(n) => n
    case Expr.Var(x) =>
      v.getOrElse(x,
        sys.error(s"Variable $x not found - only ${v.keys.mkString(",")}"))
    case Expr.Func("+", List(e1,e2)) => apply(e1) + apply(e2)
    case Expr.Func("-", List(e1,e2)) => apply(e1) - apply(e2)
    case Expr.Func("*", List(e1,e2)) => apply(e1) * apply(e2)
    case Expr.Func("/", List(e1,e2)) => apply(e1) / apply(e2)
    case Expr.Func("^", List(e1,e2)) => math.pow(apply(e1),apply(e2))
    case Expr.Func("pow", List(e1,e2)) => math.pow(apply(e1),apply(e2))
    case Expr.Func("sqrt", List(e)) => math.sqrt(apply(e))
    case Expr.Func("exp", List(e)) => math.exp(apply(e))
    case Expr.Func("sin", List(e)) => math.sin(apply(e))
    case Expr.Func("cos", List(e)) => math.cos(apply(e))
    case Expr.Func("tan", List(e)) => math.tan(apply(e))
    case Expr.Func("cosh", List(e)) => math.cosh(apply(e))
    case Expr.Func("sinh", List(e)) => math.sinh(apply(e))
    case Expr.Func("tanh", List(e)) => math.tanh(apply(e))
    case Expr.Func("ln", List(e)) => math.log(apply(e))
    case Expr.Func("pi", List()) => math.Pi
    case Expr.Func("unif",List()) => rand.nextDouble()
    case Expr.Func("unif",List(e1,e2)) => rand.between(apply(e1),apply(e2))
    case Expr.Func(op, es) =>
      sys.error(s"Cannot evaluate function ${Show(e)}")

  def apply(c:Cond)(using v:Valuation, rand:Random): Boolean = c match
    case Cond.True => true
    case Cond.False => false
    case Cond.And(c1, c2) => apply(c1) && apply(c2)
    case Cond.Or(c1, c2) => apply(c1) || apply(c2)
    case Cond.Not(c1) => !apply(c1)
    case Cond.Comp("==", e1, e2) => apply(e1) == apply(e2)
    case Cond.Comp("!=", e1, e2) => apply(e1) != apply(e2)
    case Cond.Comp(">=", e1, e2) => apply(e1) >= apply(e2)
    case Cond.Comp("<=", e1, e2) => apply(e1) <= apply(e2)
    case Cond.Comp(">", e1, e2) => apply(e1) > apply(e2)
    case Cond.Comp("<", e1, e2) => apply(e1) < apply(e2)
    case Cond.Comp(op, e1, e2) =>
      sys.error(s"Cannot evaluate condition ${Show(c)}")

  def rands(e:Expr)(using v:MValuation, rand:Random = new Random): Expr = e match {
    case Expr.Num(n) => e
    case Expr.Var(x) => e
    case Expr.Func("unif",List()) => Expr.Num(rand.nextDouble())
    case Expr.Func("unif",List(Expr.Num(n1),Expr.Num(n2))) => Expr.Num(rand.between(n1,n2))
    case Expr.Func("unif",_) => sys.error(s"Cannot evaluate a random function with variables here (${Show(e)})")
    case Expr.Func(n,es) => Expr.Func(n,es.map(rands))
  }