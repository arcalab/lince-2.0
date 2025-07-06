package lince.backend

import lince.syntax.Lince.{Action, Program}
import lince.syntax.{Lince, Show}
import Semantics.step
import lince.backend.Eval.Valuation
import lince.syntax.Lince.Program.EqDiff

import scala.annotation.tailrec

object Plot:

  // Type of intermediate structures -- experimental
  private type Traces      = Map[String,TraceVar]
  private type TraceVar    = Map[Double,Either[Double,(Double,Double)]] // time -> 1 or 2 points (if boundary)
  private type Boundaries  = Map[String,BoundaryVar]
  private type BoundaryVar = Map[Either[Double,Double],(Double,String)] // left/right of a time t -> value and comment
  private type St = Semantics.St

  /**
   * Performs discrete steps until no more step can be taken
   * @param st initial state
   * @param hist history of actions taken
   * @return Pair with the list of actions taken and the reached state
   */
  @tailrec
  def discSteps(st: St, hist: List[Action] = Nil): (List[Action], St) =
    nextStatement(st.p) match
      case _:EqDiff => hist -> st
      case _ => step(st) match
        case None => hist -> st // reached the end
        case Some((a, st2)) => discSteps(st2, a :: hist)

  def nextStatement(p:Program): Program = p match {
    case Program.Seq(Program.Seq(p1,p2), q) =>
      nextStatement(Program.Seq(p1,Program.Seq(p2,q)))
    case Program.Seq(Program.Skip, q) => nextStatement(q)
    case Program.Seq(p, q) => nextStatement(p)
    case _ => p
  }

  @tailrec
  def contSteps(st: St, timeStep: Double,
                baseTime:Double, counter:Int = 1,
                hist: List[(Double,Valuation)] = Nil): (List[(Double,Valuation)], St) =
    val goalTime = st.t min (timeStep*counter)
    nextStatement(st.p) match
      case ed:EqDiff => step(st.copy(t = goalTime)) match
        case None =>
//            println(s"[CS] no step possible using time ${st.t} MIN ${timeStep*counter}");
          hist -> st
        case Some((Action.DiffStop(_,_),st2)) => // reached goalTime
//            println(s"[CS] Diff-stop - reached the goal time (min t/ts*counter)\n   ${(baseTime+goalTime::hist) -> st2}")
          contSteps(st, timeStep, baseTime, counter+1, ((baseTime+goalTime) -> st2.v)::hist)
        case Some((Action.DiffSkip(_,timePassed),st2)) => // "diff-skip // reached duration
//            println(s"[CS] reached duration\n    FROM ${Show.simpleSt(st)}\n    BY $a\n    TO ${Show.simpleSt(st2)}")
          (((baseTime+timePassed)->st2.v)::hist) -> st2.copy(t = st.t-timePassed)
        case Some((stp,_)) => sys.error(s"Expected continuous step but found ${Show(stp)}")
      case n =>
//          println(s"[CS] No ODEs now. Next: ${Show(n)}");
          hist -> st

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

    apply(from.copy(t = maxt), stepSize, mint, "")
  }

//  @tailrec
  def apply(st: St, stepSize: Double, timePassed:Double, acc:String): String =
    var res = acc //s"$acc## Round at ${st.v} / ${st.t}\n"

    val (as, st2) = discSteps(st)
    res += s"-- ${as.mkString(",")} -->\n|  ${Show.simpleSt(st2)}\n"

    val (points,st3) = contSteps(st2, stepSize, timePassed)
    res += s"== ${points.mkString("; ")} ==>\n|  ${Show.simpleSt(st3)}\n"

    if Semantics.accepting(st3) then res + "## Finished"
    else apply(st3, stepSize, timePassed + (st2.t-st3.t), res)
//    res + "finishing"


  def applyOld(from: St, stepSize: Double, timePassed:Double): String = {
    // state while traversing
    var st = from
    var time = timePassed
    var done: Boolean = false // when to stop
    var res = s"Starting at ${st.v} / ${st.t}\n"

    // 1: go to starting point
    // st... assuming starting at 0
    // 2: traverse

    while(!done) {
      // Discrete steps
      val (msgs,st2) = discSteps(st)
      res += s">>> pre-prog: ${Show.simpleStatm(st.p)}\n"
      st = st2
      res += s"[$time] Disc to ${st.v}\n"
      res += s">>> pos-prog: ${Show.simpleStatm(st.p)}\n"
//      boundaries ++= mkStart(msgs,st2,time,boundaries)
      Semantics.step(st.copy(t = stepSize)) match
        case None =>
          res += s"[$time] Cont DONE\n"
          done = true
        case Some((a,st2)) =>
          // either stopped at final time or end of sub-traj
          res += s"passed ${stepSize - st2.t} (started at ${stepSize}, ended at ${st2.t})\n"
          time += stepSize - st2.t // add time that passed
          st = st2.copy(t = st.t-(stepSize-st2.t)) // take time that passed
          if Semantics.accepting(st) then {
            res += "accepting!!\n"
            done = true
          }
          res += s"[$time] Cont to ${st.v}\n"

      //          boundaries ++= mkEnd(a,st2,time,boundaries)
    }

    res
//    s"Finished @ ${st}\nstep: $stepSize\ntime: $time\nwith\n${boundaries.mkString("\n")}"
  }

  // not yet used - maybe not needed
  def mkStart(msgs:List[String],s:St,t:Double,bs:Boundaries): Boundaries =
    for (v,n) <- s.v yield {
      // maybe filter msgs first
      val bv: BoundaryVar = Map((Left(t)) -> (n,msgs.mkString(",")))
      v -> (bs.getOrElse(v,Map()) ++ bv)
    }

  def mkEnd(a:String,s:St,t:Double,bs:Boundaries) =
    for (v, n) <- s.v yield {
      // maybe filter msgs first
      val bv: BoundaryVar = Map((Right(t)) -> (n, ""))
      v -> (bs.getOrElse(v,Map()) ++ bv)
    }

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


