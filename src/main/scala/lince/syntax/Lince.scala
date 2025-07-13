package lince.syntax

import scala.util.Random

/**
 * Internal structure to represent terms in Lince 2.0.
 */

object Lince:

  case class Simulation(prog:Program, pi:PlotInfo)

  ///// Program ////

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

  ///// Actions ////

  enum Action:
    case Assign(v: String, n:Double)
    case DiffStop(eqs: Map[String, Expr], time: Double)
    case DiffSkip(eqs: Map[String, Expr], time: Double)
    case CheckIf(b: Cond, res:Boolean)
    case CheckWhile(b: Cond, res:Boolean)
    override def toString: String = Show(this)

  ///// Plot configuration ////

  case class PlotInfo( minTime:Double,
                       maxTime:Double,
                       maxLoops: Int,
                       samples: Int,
                       rand: Random,
                       showAll: Boolean,
                       showVar: String=>Boolean
  )
  object PlotInfo:
    val default = PlotInfo(0,10,50,20,new Random(),false,_=>true)


