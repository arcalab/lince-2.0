package lince.backend.plot

import lince.backend.BigSteps.{contSteps, discSteps}
import lince.backend.plot.Plot.{MarkedPoints, Points, Trace, Traces}
import lince.backend.{BigSteps, SmallStep}
import lince.syntax.Lince
import lince.syntax.Lince.{Action, Expr, PlotInfo, Program, Simulation}

import scala.annotation.tailrec


/**
 * Structure to compile the data that is in a plot
 * @param current maps the current trace (a segment in the plot) to each variable
 * @param traces maps the collection of finished traces to each variable
 * @param endings maps the ending of traces to each variable
 * @param beginnings maps the beginning of traces to each variable
 */
case class Plot(current: Map[String,Trace], traces: Map[String,Traces],
                endings: Map[String,Points], beginnings: Map[String,MarkedPoints]):
  /** Adds a new point to the current plot. Usage: `plot + ("x" -> now -> value)` */
  def +(varTimeValue:((String,Double),Double)): Plot =
    val ((x,time),value) = varTimeValue
    val oldTr: Trace = current.getOrElse(x,Nil)
    this.copy(current = current + (x -> ((time -> value) :: oldTr)))

  /** Ends the current trace, adding the last point to the ending markers. */
  private def endTrace(x:String): Plot =
    current.getOrElse(x,Nil).headOption match
      case Some(lastValue) =>
        this.copy(current = current - x, //+ (x -> Nil),
                  traces  = traces +  (x -> (current.getOrElse(x,Nil)::traces.getOrElse(x,Nil))),
                  endings = endings + (x -> (lastValue::endings.getOrElse(x,Nil))))
      case None => this

  @tailrec
  final def endTraces: Plot =
    current.headOption match {
      case Some((v,trace)) => endTrace(v).endTraces
      case None => this
    }

  /** Starts a new trace, ending a possible previous one, adding the beginning and ending markers.  */
  private def startTrace(x: String, time: Double, value: Double, act:List[Action]): Plot =
    endTrace(x)
      .copy(beginnings = beginnings + (x -> ((time,value,act)::beginnings.getOrElse(x,Nil))),
            current = current + (x -> List(time->value)))

  def show: String =
    (for x <- (current.keys ++ traces.keys ++ endings.keys ++ beginnings.keys) yield
      s"$x:\n    cur: ${
        current.getOrElse(x,Nil).mkString(",")}\n    trc: ${
        traces.getOrElse(x,Nil).map(x => x.mkString(",")).mkString("\n         ")}\n    end: ${
        endings.getOrElse(x,Nil).mkString(",")}\n    begin: ${
        beginnings.getOrElse(x,Nil).mkString(",")}")
      .mkString("\n")


object Plot:

  type Traces = List[Trace]
  type Trace  = List[(Double,Double)]
  type Points = List[(Double,Double)]
  type MarkedPoints = List[(Double,Double,List[Action])]
  def empty = Plot(Map(),Map(),Map(),Map())

  private type St = SmallStep.St

  def allPlots(st:St, pinfo:PlotInfo): List[(Plot,PlotInfo)] =
    for run <- (1 to pinfo.runs).toList yield
      val pi2 = pinfo.copy(runs = run)
      (apply(Simulation(st.p,pi2).state, pi2),pi2)

  def apply(st:St, pinfo:PlotInfo): Plot =
    apply(st, pinfo.minTime, pinfo.maxTime, pinfo.samples, pinfo.showAll, pinfo.showVar)

  /**
   *  Calculate a plot by traversing the state-space while collecting points and action names.
   * @param st initial state
   * @param from starting time
   * @param to ending time
   * @param samples number of time points to sample (other than the starting point),
   * @param showCont true if the borders of all subtrajectories should be marked, instead of just when there is an assignment between subtrajectories
   * @param filter given a variable name, returns true if the variable should be included in the plot
   * @return the Plot containing the points and action names while traversing a hybrid program
   */
  def apply(st: St, from: Double, to: Double,
            samples:Int=50, showCont:Boolean=false,filter:String=>Boolean): Plot = {

    // need to traverse my trajectory
    // need a maxt
    val maxt: Double = to min st.t
    // need a step size
    val stepSize: Double = (maxt - from) / samples

    val stInit = if from!=0
      then valToAssign(BigSteps.bigStep(st.copy(t=from))._2.copy(t=maxt-from))
      else st.copy(t = maxt)

//    val stInit = st.copy(t = maxt) // need to start after navigating to time mint!
                                 // need bigstep to mint.
//    apply(st, stepSize, mint, "")
    calcPlot(stInit, stepSize, from, showCont, Plot.empty, filter).endTraces
  }

  /** Converts the state of a program (given by the values of the variables) into an introductory sequence of assignments. */
  private def valToAssign(st:St): St =
    val assign = for (x,value) <- st.v yield Program.Assign(x,Expr.Num(value))
    st.copy(p = assign.foldRight(st.p)(Program.Seq.apply), v = Map())

  /**
   * Main function that produces the plot: at each run performs a collection of
   * discrete and a collection of continuous steps, untin the end or bound is reached.
   * @param st initial state
   * @param stepSize time interval to sample
   * @param timePassed time that passed at the beginning
   * @param acc accumulated plot
   * @return plot from the run
   */
  @tailrec
  def calcPlot(st: St, stepSize: Double, timePassed: Double, showCont:Boolean, acc: Plot, filter:String=>Boolean): Plot =
    var res = acc
    // run discrete steps
    val (as, st2) = discSteps(st)
    // update Plot
    val setVars = if showCont
      then st2.v.keySet
      else for (case Action.Assign(v,_) <- as.toSet) yield v
    for (v <- setVars if filter(v)) do
      res = res.startTrace(v,
        timePassed,
        st2.v.getOrElse(v,sys.error(s"No value for ${v} after ${as.mkString(",")}")),
        as
      )

    // run continuous steps while sampling
    val (points, st3) = contSteps(st2, stepSize, timePassed)
    // update Plot
    for ((time,valuation) <- points.reverse; (x,value) <- valuation if filter(x)) do
      res = res + (x -> time -> value)

    if SmallStep.accepting(st3) || st == st3 then  res // res + "## Finished"
    else calcPlot(st3, stepSize, timePassed + (st2.t - st3.t), showCont, res, filter)





