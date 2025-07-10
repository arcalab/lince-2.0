package lince.backend

import lince.syntax.Lince.{Action, Program}
import lince.syntax.{Lince, Show}
import BigSteps.{bigStep, contSteps, discSteps, nextStatement}
import lince.backend.Plot.{Points, Trace, Traces, MarkedPoints}

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

  // Type of intermediate structures -- experimental
//  private type Traces      = Map[String,TraceVar]
//  private type TraceVar    = Map[Double,Either[Double,(Double,Double)]] // time -> 1 or 2 points (if boundary)
//  private type Boundaries  = Map[String,BoundaryVar]
//  private type BoundaryVar = Map[Either[Double,Double],(Double,String)] // left/right of a time t -> value and comment

  private type St = SmallStep.St

  def apply(from:
            St, divName:String, range:Option[(Double,Double)]=None,
            samples:Int=50, showCont:Boolean=false): String = {

    // need to traverse my trajectory
    // need mint and maxt
    val mint: Double = range.map(_._1).getOrElse(0)
    val maxt: Double = range.map(_._2).getOrElse(from._3)
    // need a step size
    val stepSize: Double = (maxt - mint) / samples
    // alternate: discrete step (collect notes + starting), timed step (collect ending)

    val st = from.copy(t = maxt) // need to start after navigating to time mint!
                                 // need bigstep to mint.
//    apply(st, stepSize, mint, "")
    apply(st, stepSize, mint, Plot.empty).endTraces.show  //endTraces.show
  }


  @tailrec
  def apply(st: St, stepSize: Double, timePassed: Double, acc: Plot): Plot =
    var res = acc
    println("a")
    val (as, st2) = discSteps(st)
    println("b")
    // update Plot
    val setVars = for (case Action.Assign(v,_) <- as.toSet) yield v
    for (v <- setVars) do
      res = res.startTrace(v,
        timePassed,
        st2.v.getOrElse(v,sys.error(s"No value for ${v} after ${as.mkString(",")}")),
        as
      )
    println("c")

    val (points, st3) = contSteps(st2, stepSize, timePassed)
    println("d")
    // update Plot
    for ((time,valuation) <- points.reverse; (x,value) <- valuation) do
      res = res + (x -> time -> value)
    println("e")

    if SmallStep.accepting(st3) || st == st3 then  res // res + "## Finished"
    else apply(st3, stepSize, timePassed + (st2.t - st3.t), res)
  //    res + "finishing"


  def plotToJS(plot: Plot,divName:String): String = {
    colours = (0,Map())
    s"""var colors = Plotly.d3.scale.category10();
       |${traceToJS(plot.traces)}
       |
       |${markEndings(plot.endings)}
       |
       |${markBeginning(plot.beginnings)}
       |
       |var data = [${(plot.traces.keys.map("t_"+_).toList ++
                       plot.endings.keys.map("end_"+_).toList ++
                       plot.beginnings.keys.map("beg_"+_).toList
                      ).mkString(",")}];
       |var layout = {hovermode:'closest'};
       |Plotly.newPlot('$divName', data, layout, {showSendToCloud: true});
       |""".stripMargin

    //    val traceNames = traces.keys.map("t_"+_).toList ++
    //                     boundaries.keys.map("b_out_"+_).toList ++
    //                     boundaries.keys.map("b_in_"+_).toList ++
    //                     boundaries.keys.map("w_"+_).toList
    //
    //
    //    js += s"var data = ${traceNames.mkString("[",",","]")};" +
    //      s"\nvar layout = {hovermode:'closest'};" +
    //      s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"
    //
  }

  private var colours: (Int,Map[String,Int]) = (0,Map())
  def colour(x:String) = colours._2.get(x) match
    case Some(i) => i
    case None =>
      val oldCol = colours._1
      colours = (oldCol+1,colours._2+(x->oldCol))
      oldCol


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

    //    for ((variable, values) <- boundaries) {
    //      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
    //      js += mkMarkers(variable,"out",outs, // open circles
    //        s"""{color: 'rgb(255, 255, 255)',
    //           | size: 10,
    //           | line: {
    //           |   color: colors(${colorIDs.getOrElse(variable, 0)}),
    //           |   width: 2}}""".stripMargin)
    //      js += mkMarkers(variable,"in",ins, // filled circles
    //        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
    //           | size: 10,
    //           | line: {
    //           |   color: colors(${colorIDs.getOrElse(variable, 0)}),
    //           |   width: 2}}""".stripMargin)
    //    }
    //    js  }

  //  def applyOld(from: St, stepSize: Double, timePassed:Double): String = {
//    // state while traversing
//    var st = from
//    var time = timePassed
//    var done: Boolean = false // when to stop
//    var res = s"Starting at ${st.v} / ${st.t}\n"
//
//    // 1: go to starting point
//    // st... assuming starting at 0
//    // 2: traverse
//
//    while(!done) {
//      // Discrete steps
//      val (msgs,st2) = discSteps(st)
//      res += s">>> pre-prog: ${Show.simpleStatm(st.p)}\n"
//      st = st2
//      res += s"[$time] Disc to ${st.v}\n"
//      res += s">>> pos-prog: ${Show.simpleStatm(st.p)}\n"
////      boundaries ++= mkStart(msgs,st2,time,boundaries)
//      SmallStep.step(st.copy(t = stepSize)) match
//        case None =>
//          res += s"[$time] Cont DONE\n"
//          done = true
//        case Some((a,st2)) =>
//          // either stopped at final time or end of sub-traj
//          res += s"passed ${stepSize - st2.t} (started at $stepSize, ended at ${st2.t})\n"
//          time += stepSize - st2.t // add time that passed
//          st = st2.copy(t = st.t-(stepSize-st2.t)) // take time that passed
//          if SmallStep.accepting(st) then {
//            res += "accepting!!\n"
//            done = true
//          }
//          res += s"[$time] Cont to ${st.v}\n"
//
//      //          boundaries ++= mkEnd(a,st2,time,boundaries)
//    }
//
//    res
////    s"Finished @ ${st}\nstep: $stepSize\ntime: $time\nwith\n${boundaries.mkString("\n")}"
//  }






//  // not yet used - maybe not needed
//  def mkStart(msgs:List[String],s:St,t:Double,bs:Boundaries): Boundaries =
//    for (v,n) <- s.v yield {
//      // maybe filter msgs first
//      val bv: BoundaryVar = Map((Left(t)) -> (n,msgs.mkString(",")))
//      v -> (bs.getOrElse(v,Map()) ++ bv)
//    }
//
//  def mkEnd(a:String,s:St,t:Double,bs:Boundaries) =
//    for (v, n) <- s.v yield {
//      // maybe filter msgs first
//      val bv: BoundaryVar = Map(Right(t) -> (n, ""))
//      v -> (bs.getOrElse(v,Map()) ++ bv)
//    }

  //
//
//    val dur = traj.getDur
//
//    // trick to avoid many sampling when already lots of boundaries exist
//    val nbrSamples = 0.max(100 - traj.getInits.getOrElse(Map()).size)
//
//
//    implicit val rand:()=>Double = ()=>
//      sys.error("Unsupported: continuous points in a trajectory should not have random functions.")
//
//    val max: Double = Eval(dur.getOrElse(SVal(10)),0)
//
//    val colorIDs: Map[String,Int] =
//      traj.getVars.zipWithIndex.toMap
//
//    // traces are mappings from variables t0 lists of "y" values
//    var traces:Traces          = Map().withDefaultValue(Map())
//   // each point in time has 1 or 2 values
//    var boundaries: Boundaries = Map().withDefaultValue(Map())
//   // each boundary variable maps begin or end times to a value and a note.
//
//    // Generate sampling values for time - 100 values from 0 to max
//    val (start,end) = range match {
//      case Some((s,e)) =>
//        if (dur.isDefined)
//          (s.max(0).min(max),e.max(0).min(max))
//        else
//          (s max 0, e max 0)
//      case None => if (max<=0) (0.0,0.0) else (0.0,max)
//    }
//    val samples =
//      if (nbrSamples==0)
//        Nil
//      else if ((end-start)<=0)
//        List(SVal(start))
//      else
//        SVal(start) :: (0 to nbrSamples).toList.map(_=> SDiv(SSub(SVal(end),SVal(start)),SVal(nbrSamples)))
//           // to end by ((end-start) / 10)
//
//    // checks if a time value is within the scope
//    def inScope(t:Double): Boolean = t>=start && t<=end
//
//
//    val sampleValues = traj.evalBatch(SVal(start),SVal(end), SDiv(SSub(SVal(end),SVal(start)),SVal(nbrSamples))) //(samples)
//
//
//    for ((t,x) <- sampleValues; (variable,value) <- x)
//      traces += variable -> (traces(variable) + (Eval(t)->Left(Eval(value))))
//    // Add ending points to "boundaries" and to "traces"
//    for (e <- traj.getEnds;
//         (t,endValues) <- e if inScope(Eval(t,0));
//         (variable,endValue) <- endValues) {
//      val t2 = Eval(t,0) // evaluate the time expression of next point
//      val endValue2 = Eval(endValue,t2) // evaluate value for that point
//
//      traces     += variable -> (traces(variable)     + (t2->Left(endValue2)))
//      boundaries += variable -> (boundaries(variable) + (Left(t2)->(endValue2,"")))
//    }
//
//    // add init points to boundaries and traces
//    for (is <- traj.getInits;
//         (t,initValues) <- is if inScope(Eval(t,0));
//         (variable,initValue) <- initValues) {
//      val t2 = Eval(t,0)
//      val initValue2 = Eval(initValue,t2)
//
//      val nextTraceT = traces(variable).get(t2) match {
//        case Some(Left(v)) => Right(v,initValue2)
//        case Some(x) => x
//        case None => Left(initValue2)
//      }
//      traces     += variable -> (traces(variable)     + (t2->nextTraceT))
//      boundaries += variable -> (boundaries(variable) + (Right(t2)->(initValue2,"")))
//    }
//
//    // adding notes to boundary points
//    for (n <- traj.getNotes ; (expr,note) <- n) {
//      val t = Eval(expr)
//      if (inScope(t))
//        boundaries = boundaries.view.mapValues(vals => addNote(t,note,vals)).toMap
//    }
//    def addNote(t: Double, n: String, vals: BoundaryVar): BoundaryVar = {
//      vals.get(Right(t)) match {
//        case Some((value,n2)) => vals + (Right(t)->(value,n2++n++"<br>"))
//        case None => vals.get(Left(t)) match {
//          case Some((value,n2)) => vals + (Left(t) -> (value,n2++n++"<br>"))
//          case None => vals + (Left(t)->(0,n)) // should not happen...
//        }
//      }
//    }
//
//    // clean boundaries in continuous points
//    if (hideCont) {
//      boundaries = boundaries.view.mapValues(filterCont).toMap
//    }
//
//    /////
//    // Build the JavaScript code to generate graph
//    /////
//    var js = "var colors = Plotly.d3.scale.category10();\n"
//
//
//    js += buildTraces(traces,colorIDs)
//    js += buildBoundaries(boundaries,colorIDs)
//    js += buildWarnings(traj,inScope,colorIDs)
//
//
//    val traceNames = traces.keys.map("t_"+_).toList ++
//                     boundaries.keys.map("b_out_"+_).toList ++
//                     boundaries.keys.map("b_in_"+_).toList ++
//                     boundaries.keys.map("w_"+_).toList
//
//
//    js += s"var data = ${traceNames.mkString("[",",","]")};" +
//      s"\nvar layout = {hovermode:'closest'};" +
//      s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"
//
//    js
//  }
//
//
//  ///////
//
//  private def buildTraces(traces: Traces, colorIDs: Map[String, Int]): String = {
//    var js = ""
//    for ((variable, values) <- traces) {
//      val tr = values.toList.sortWith(_._1 <= _._1).flatMap(expandPoint)
//      val (xs,ys) = tr.unzip
//      js +=
//        s"""var t_$variable = {
//           |   x: ${xs.mkString("[",",","]")},
//           |   y: ${ys.mkString("[",",","]")},
//           |   mode: 'lines',
//           |   line: {color: colors(${colorIDs.getOrElse(variable,0)})},
//           |   legendgroup: 'g_${remove_variable(variable)}',
//           |   name: '${remove_variable(variable)}'
//           |};
//             """.stripMargin
//    }
//    js
//  }
//
//  private def buildBoundaries(boundaries: Boundaries, colorIDs: Map[String, Int]): String = {
//    var js = ""
//    for ((variable, values) <- boundaries) {
//      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
//      js += mkMarkers(variable,"out",outs, // open circles
//        s"""{color: 'rgb(255, 255, 255)',
//           | size: 10,
//           | line: {
//           |   color: colors(${colorIDs.getOrElse(variable, 0)}),
//           |   width: 2}}""".stripMargin)
//      js += mkMarkers(variable,"in",ins, // filled circles
//        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
//           | size: 10,
//           | line: {
//           |   color: colors(${colorIDs.getOrElse(variable, 0)}),
//           |   width: 2}}""".stripMargin)
//    }
//    js
//  }
//
//  private def buildWarnings(traj: Traj, inScope:Double=>Boolean,
//                            colorIDs: Map[String, Int])(implicit rand:()=>Double): String = {
//    var js = ""
//    for (variable <- traj.getVars) {
//      js += mkWarnings(variable,traj,inScope,
//        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
//           | size: 15,
//           | line: {
//           |   color: 'rgb(0,0,0)',
//           |   width: 2}}""".stripMargin)
//    }
//    js
//  }
//
// def remove_variable(variable:String):String = {
//  //var aux=variable.substring(0,variable.length-1)
//  var aux=variable.substring(1,variable.length)
//  return aux
// }
//
//  private def expandPoint(point:(Double,Either[Double,(Double,Double)])): List[(Double,String)] =
//    point match {
//      case (t,Left(v)) => List((t,v.toString))
//      case (t,Right((v1,v2))) => List((t,v1.toString),(t,"null"),(t,v2.toString))
//    }
//
//  private def filterCont(boundary: BoundaryVar): BoundaryVar = {
//    boundary.filter({
//      case (Left(t),v1)  => boundary.get(Right(t)) match {
//          case Some(v2) => v1._1 != v2._1
//          case None     => true
//        }
//      case (Right(t),v1) => boundary.get(Left(t)) match {
//          case Some(v2) => v1._1 != v2._1
//          case None     => true
//        }
//    })
//  }
//
//  private def mkMarkers(variable:String,
//                        inout:String,
//                        data:List[(Either[Double,Double],(Double,String))],
//                        style: String): String =
//    s"""var b_${inout}_$variable = {
//       |   x: ${data.map(_._1.fold(x=>x,x=>x)).mkString("[", ",", "]")},
//       |   y: ${data.map(_._2._1).mkString("[", ",", "]")},
//       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
//       |   mode: 'markers',
//       |   marker: $style,
//       |   type: 'scatter',
//       |   legendgroup: 'g_${remove_variable(variable)}',
//       |   name: 'boundary of ${remove_variable(variable)}',
//       |   showlegend: false
//       |};""".stripMargin
//
////  marker: {color: colors(${colorID.getOrElse(variable, 0)})},
//
//  private def mkWarnings(variable: String, traj: Traj
//                       , inScope: Double=>Boolean
//                       , style:String)(implicit rand:()=>Double): String = {
//
//    (traj.getWarnings,traj.getInits,traj.getEnds) match {
//      case (Some(warns),Some(inits),Some(ends)) =>
//        val values = (ends ++ inits).map(kv => Eval(kv._1) -> kv._2)
//        val (x,y,msg) = warns
//          .toList
//          .map(es => (Eval(es._1, 0), "'" + fixStr(es._2) + "'"))
//          .filter(es => inScope(es._1))
//          .sorted
//          .map(warn=>(warn._1, Eval(
//            values.getOrElse(warn._1,Map():ValuationSyExpr) // get Valuation at warning warn
//                  .getOrElse(variable, SVal(0)) // get expression of Variable
//            ), warn._2))
//          .unzip3
//
//        s"""var w_$variable = {
//           |   x: ${x.mkString("[",",","]")},
//           |   y: ${y.mkString("[",",","]")},
//           |   text: ${msg.mkString("[",",","]")},
//           |   mode: 'markers',
//           |   marker: $style,
//           |   type: 'scatter',
//           |   legendgroup: 'g_${remove_variable(variable)}',
//           |   name: 'Warning',
//           |   showlegend: false
//           |};""".stripMargin
//
//
//      case _ => s"var w_$variable = {};"
//      }
//  }

  private def fixStr(str:String): String =
    str.replaceAll("\\\\", "\\\\\\\\")


