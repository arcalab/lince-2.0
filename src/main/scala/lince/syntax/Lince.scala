package lince.syntax

import lince.backend.SmallStep

import scala.util.Random

/**
 * Internal structure to represent terms in Lince 2.0.
 */

object Lince:

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

  case class Simulation(prog:Program, pi:PlotInfo):
    def state = SmallStep.St(prog,Map(),pi.seed+(pi.runs-1), pi.maxTime,pi.maxLoops)

  case class PlotInfo( minTime:Double,
                       maxTime:Double,
                       maxLoops: Int,
                       samples: Int,
                       seed: Long,
                       showAll: Boolean,
                       showVar: String=>Boolean,
                       height: Int, // height in px
                       runs: Int, // number of times to repeat the run
                       portrait: Option[(String,String)], // to change the variables in the x and y axis
  )
  object PlotInfo:
    def default = PlotInfo(0,10,500,40,SmallStep.rand.nextLong(),false,_=>true,450,1,None)


