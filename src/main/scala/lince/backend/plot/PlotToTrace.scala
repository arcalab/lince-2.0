package lince.backend.plot

import lince.backend.plot.Plot.{MarkedPoints, Points, Traces, Trace}
import lince.backend.Eval.Valuation
import lince.syntax.Lince.PlotInfo

/**
 * Builds JavaScript code to generate Plotly graphs from a given plot
 */
object PlotToTrace:

  /** Converts a plot to JavaScript instructions for Plotly */
  def apply(plot: Plot ): List[(Double,Valuation)] =
    var res: Map[Double,Valuation] =  Map()
    res = loadTraces(plot.traces,res)
    res = loadEndings(plot.endings,res)
    res = loadBeginnings(plot.beginnings,res)
    res.toList.sortBy(_._1) 
  
  private def loadTraces(traces: Map[String,Traces], acc: Map[Double,Valuation]): Map[Double,Valuation] =
    var res = acc
    for (v,trs) <- traces
        tr <- trs
        (t,value) <- tr do 
      res = res + (t -> (res.getOrElse(t,Map()) + (v->value)))
    res

  private def loadEndings(endings: Map[String,Points], acc: Map[Double,Valuation]): Map[Double,Valuation] =
    var res = acc
    for (v,pts) <- endings
        (t,value) <- pts do
      res = res + (t -> (res.getOrElse(t,Map()) + (v->value)))
    res

  private def loadBeginnings(beginnings: Map[String,MarkedPoints], acc: Map[Double,Valuation]): Map[Double,Valuation] =
    var res = acc
    for (v,mpts) <- beginnings
        (t,value,_) <- mpts do
      res = res + (t -> (res.getOrElse(t,Map()) + (v->value)))
    res
