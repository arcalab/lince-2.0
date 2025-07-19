package lince.backend

import lince.syntax.Lince.{Action, Expr, PlotInfo, Program}
import lince.syntax.{Lince, Show}
import BigSteps.{bigStep, contSteps, discSteps, nextStatement}
import lince.backend.Plot.{MarkedPoints, Points, Trace, Traces}

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

  def apply(st:St, divName:String, pinfo:PlotInfo): Plot =
    apply(st, pinfo.minTime, pinfo.maxTime, divName, pinfo.samples, pinfo.showAll, pinfo.showVar)

  def apply(st: St, from: Double, to: Double, divName:String,
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

  def valToAssign(st:St): St =
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

    val (points, st3) = contSteps(st2, stepSize, timePassed)
    // update Plot
    for ((time,valuation) <- points.reverse; (x,value) <- valuation if filter(x)) do
      res = res + (x -> time -> value)

    if SmallStep.accepting(st3) || st == st3 then  res // res + "## Finished"
    else calcPlot(st3, stepSize, timePassed + (st2.t - st3.t), showCont, res, filter)


  ////////////////////////////////////
  // Build JavaScript from the plot //
  ////////////////////////////////////

  /** Converts a plot to JavaScript instructions for Plotly */
  def plotToJS(plot: Plot, divName: String, pi: PlotInfo): String =
    colours = (0,Map())
    val vars = plot.traces.keys.toList.sorted
    s"""var colors = Plotly.d3.scale.category10();
       |${traceToJS(plot.traces)}
       |
       |${markEndings(plot.endings)}
       |
       |${markBeginning(plot.beginnings)}
       |
       |var data = [${(//plot.traces.keys.map("t_"+_).toList ++
                       vars.map("t_"+_) ++
                       plot.endings.keys.map("end_"+_).toList ++
                       plot.beginnings.keys.map("beg_"+_).toList
                      ).mkString(",")}];
       |var layout = {hovermode:'closest',
       |   xaxis: {title: {text: 'time' } },
       |   yaxis: {title: {text: '${vars.mkString("/")}' } },
       |   height: ${pi.height}
       |};
       |Plotly.newPlot('$divName', data, layout, {showSendToCloud: true});
       |""".stripMargin

  /** Auxiliar function to use consistent colours for all elements to the same variable. */
  private var colours: (Int,Map[String,Int]) = (0,Map())
  def colour(x:String) = colours._2.get(x) match
    case Some(i) => i
    case None =>
      val oldCol = colours._1
      colours = (oldCol+1,colours._2+(x->oldCol))
      oldCol

  /** Converts the traces (lines) to JS for Plotly. */
  def traceToJS(tr: Map[String,Traces]): String =
    var js = ""
    for (variable,traces) <- tr do
      val tr = traces.map(tr =>tr.head.copy(_2 = "null")::tr).flatten.tail
      val (xs,ys) = tr.unzip
      js +=
        s"""var t_$variable = {
           |   x: ${xs.mkString("[",",","]")},
           |   y: ${ys.mkString("[",",","]")},
           |   mode: 'lines',
           |   line: {color: colors(${colour(variable)})},
           |   legendgroup: 'g_${variable}',
           |   name: '${variable}'
           |};
           |""".stripMargin
    js

  /** Converts the beginning markers to JS for Plotly. */
  def markBeginning(ps: Map[String,MarkedPoints]): String =
    var js = ""
    for (variable, points) <- ps do {
      val (xs, ys, acts) = points.unzip3
      js +=
        s"""var beg_${variable} = {
           |    x: ${xs.map(x => s"$x,$x").mkString("[", ",", "]")},
           |    y: ${ys.mkString("[", ",null,", "]")},
           |    text: [${acts.map(x=>s"'${x.reverse.mkString("<br>")}'").mkString(",null,")}],
           |    mode: 'markers',
           |    marker: {color: colors(${colour(variable)}),
           |      size: 10,
           |      line: {
           |        color: colors(${colour(variable)}),
           |        width: 2
           |    }},
           |    type: 'scatter',
           |    legendgroup: 'g_${variable}',
           |    name: 'beginning of ${variable}',
           |    showlegend: false,
           |};
           |""".stripMargin
    }
    js

  /** Converts the ending markers to JS for Plotly. */
  def markEndings(ps: Map[String,Points]): String =
    var js = ""
    for (variable,points) <- ps do {
      val (xs, ys) = points.unzip
      js +=
        s"""var end_${variable} = {
           |    x: ${xs.map(x => s"$x,$x").mkString("[", ",", "]")},
           |    y: ${ys.mkString("[", ",null,", "]")},
           |    text: [],
           |    mode: 'markers',
           |    marker: {color: 'rgb(255, 255, 255)',
           |      size: 10,
           |      line: {
           |        color: colors(${colour(variable)}),
           |        width: 2
           |    }},
           |    type: 'scatter',
           |    legendgroup: 'g_${variable}',
           |    name: 'ending of ${variable}',
           |    showlegend: false,
           |};
           |""".stripMargin
    }
    js


