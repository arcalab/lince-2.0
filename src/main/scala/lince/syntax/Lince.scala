package lince.syntax

import lince.backend.{SmallStep, BasicSmallStep, ConcurrentSmallStep, Stream}

import lince.backend.Stream.RandomStrm

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
    case StreamDef(v:String, s:Stream)
    case EqDiff(eqs:Map[Location,Expr], dur:Option[Expr])
    case Seq(p:Program, q:Program)
    case ITE(b:Expr, pt:Program, pf:Program)
    case While(b:Expr, p:Program)

  enum Expr:
    case Num(n:Double)
    case True
    case False
    case Var(x:Location)
    case Func(op:String, es:List[Expr])

  ///// Actions ////

  enum Action:
    case Assign(v: Location, n:Double)
    case StrmDef(v: String, s:Stream)
    case DiffStop(eqs: Map[Location, Expr], time: Double)
    case DiffSkip(eqs: Map[Location, Expr], time: Double)
    case CheckIf(b: Expr, res:Boolean)
    case CheckWhile(b: Expr, res:Boolean)
    override def toString: String = Show(this)

  ///// Plot configuration ////

  case class Simulation(progs:Map[String, Program], pi:PlotInfo):
    def state: SmallStep.St =
      val initialStreams = Map("unif" -> RandomStrm( pi.seed + (pi.runs - 1)))
      if progs.size == 1 && progs.contains("")
      then
        SmallStep.St.Basic(
          BasicSmallStep.BasicState(
            p = progs(""),
            v = Map(),
            s = initialStreams,
            t = pi.maxTime,
            lp = pi.maxLoops
          )
        )
      else
        SmallStep.St.Concurrent(
          ConcurrentSmallStep.ConcurrentState(
            progs = progs,
            v = Map(),
            s = initialStreams,
            t = pi.maxTime,
            lp = pi.maxLoops,
            nextProcess = 0
          )
        )

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
                       monSampleFreq: Double, // frequency of sampling for the monitor (in time units)
                       monSampleNoise: Double, // noise to add to the monitor sampling time (e.g., 0.1 means that sampling time is uniformly distributed in [t-0.1, t+0.1])
  )
  object PlotInfo:
    def default = PlotInfo(0,10,500,40,100,
      (new scala.util.Random).nextLong(),
      false,_=>true,450,1,Nil,1,0)


