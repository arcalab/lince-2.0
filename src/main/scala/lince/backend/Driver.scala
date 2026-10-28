package lince.backend

import lince.syntax.Lince
import caos.sos.SOS
import Eval.Valuation
import Lince.Program.*

import SmallStep.St as SSt
import Driver.St as DSt
import Driver.{Acts,Trace}

trait Driver extends SOS[Acts,DSt]:
  // Idea for later: extend with history, to improve performance:
  //   - store history of previous steps (step fwd+bwd)
  //   - store STLs thecked in the past

  // to override: how to do a step forward in time
  def step(s:DSt): Option[(Acts,DSt)]

  // to override: how to start
  def initial(si: Lince.Simulation): DSt
  
  override def accepting(s:DSt): Boolean =
    //SmallStep.accepting(s.sst.p) || s.sst.t<=0 || s.sst.lp<=0
    SmallStep.accepting(s.sst)

  override def next[A>:Acts](s:DSt): Set[(A,DSt)] =
    step(s).toSet

  def getTrace(sim: Lince.Simulation): Trace =
    getTrace(initial(sim))

  def getTrace(init: DSt): Trace =
    var trs: List[DSt] = List(init) //next(init).map(_._2)
    var more: List[DSt] = next(init).map(_._2).toList //trs.flatMap(next).map(_._2) -- trs
    while (more.nonEmpty) do
      trs = trs ::: more
      more = more.flatMap(next).map(_._2)
    trs.map(s => (s.t,s.v))
        


object Driver:
  type Acts = List[Lince.Action]
  type Trace = List[(Double,Valuation)]

  case class St(sst:SSt,
                v:Valuation,
                t:Double)

  def initial(si: Lince.Simulation): DSt =
    if si.pi.minTime == 0 then
      DSt(SmallStep.initial(si),Map(),0)
    else
      val ss2 = SmallStep.initial(si).copy(t = si.pi.minTime)
      val (_,ss3) = BigSteps.bigStep(ss2)(using si.pi.rkSamples)
      val ss4 = ss3.copy(t = si.pi.maxTime - si.pi.minTime)
      DSt(ss4,ss4.v,si.pi.minTime)

  def initialPlus(si: Lince.Simulation): DSt = 
    if si.pi.minTime == 0 then
      val init =  SmallStep.initial(si)
      val (_,st2) = BigSteps.discSteps(init)(using si.pi.rkSamples)
      DSt(st2,st2.v,0)
    else initial(si)


  def prettyTr(tr: Trace,
               showVar:String=>Boolean = (_=>true)) =
    def roundf(x:Double) =
      BigDecimal(x).setScale(3, BigDecimal.RoundingMode.HALF_UP)
    tr
      .map(kv=>roundf(kv._1).toString + ": " +
                kv._2
                  .filter(kv => showVar(kv._1))
                  .map(x => s"${x._1} -> ${roundf(x._2)}")
                  .mkString(", "))
      .mkString("\n")



  //// Small Step Driver
  class SmallStepDriver(val rkSamples:Int) extends Driver:
    override def initial(si: Lince.Simulation): DSt =
      Driver.initial(si)

    override def step(s:DSt): Option[(Acts,DSt)] =
      SmallStep.step(s.sst)(using rkSamples).map{
        case (a,sst2) => List(a) -> s.copy(sst = sst2,
                                           v = sst2.v,
                                           t = s.t + (s.sst.t-sst2.t))
      }


  // Big Step Driver
  class BigStepDriver(val rkSamples: Int) extends Driver:
    override def initial(si: Lince.Simulation): DSt =
      Driver.initial(si)

    override def step(s:DSt): Option[(Acts,DSt)] =
      BigSteps.discSteps(s.sst)(using rkSamples) match
        // Discrete Steps were not possible, try continuous steps
        case (Nil,_) =>
          // contSteps(st: St, timeStep: Double, baseTime:Double)(using rkSamples: Int): (Trace, St) =
          SmallStep.step(s.sst)(using rkSamples) match
            case None => None // no more steps possible
            case Some((act, sst2)) =>
              Some(List(act) -> s.copy(sst = sst2,
                                      v = sst2.v,
                                      t = s.t + (s.sst.t-sst2.t)))

        // Discrete Steps were possible, return them
        case (a,sst2) => Some(a -> s.copy(sst = sst2, v = sst2.v, t = s.t))


  class TimeStepDriver(val timestep: Double, val rkSamples: Int)
        extends Driver:
    override def initial(si: Lince.Simulation): DSt =
      Driver.initialPlus(si)

    override def step(s:DSt): Option[(Acts,DSt)] =
      val newT = s.sst.t.min(timestep)
      val ss2 = s.sst.copy(t = newT)
      val (acts,ss3) = BigSteps.bigStep(ss2,Nil)(using rkSamples)
      if acts.isEmpty then None else
        val s4 = s.copy(sst = ss3.copy(t = s.sst.t - newT),
                        v = ss3.v,
                        t = s.t + newT)
        Some(acts.reverse -> s4)

