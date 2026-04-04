package lince.syntax

import lince.backend.SmallStep

import scala.util.Random

/**
 * Internal structure to represent terms in Lince 2.0.
 */

object Lince:

  case class Location(prog: Option[String], name: String):
    override def toString: String =
      prog match
        case None => name
        case Some(p) => s"$p.$name"

  ///// Program ////

  enum Program:
    case Skip
    case Assign(v:Location, e:Expr)
    case EqDiff(eqs:Map[Location,Expr], dur:Expr)
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
    case Var(x:Location)
    case Func(op:String, es:List[Expr])

  ///// Actions ////

  enum Action:
    case Assign(v: Location, n:Double)
    case DiffStop(eqs: Map[Location, Expr], time: Double)
    case DiffSkip(eqs: Map[Location, Expr], time: Double)
    case CheckIf(b: Cond, res:Boolean)
    case CheckWhile(b: Cond, res:Boolean)
    override def toString: String = Show(this)

  ///// Plot configuration ////

  case class Simulation(progs:Map[String, Program], pi:PlotInfo):
    def state = SmallStep.St(progs, Map(), pi.seed+(pi.runs-1), pi.maxTime,pi.maxLoops)

  object Simulation:
    def apply(prog:Program, pi:PlotInfo): Simulation =
      Simulation(Map(""->prog), pi)

  case class PlotInfo( minTime:Double,
                       maxTime:Double,
                       maxLoops: Int,
                       samples: Int,
                       rkSamples: Int,
                       seed: Long,
                       showAll: Boolean,
                       showVar: String=>Boolean,
                       height: Int, // height in px
                       runs: Int, // number of times to repeat the run
                       portrait: List[(String,String)], // to change the variables in the x and y axis
  )
  object PlotInfo:
    def default = PlotInfo(0,10,500,40,100,SmallStep.rand.nextLong(),false,_=>true,450,1,Nil)


