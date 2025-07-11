package lince.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import lince.backend.*
import lince.syntax.Lince.{Action, PlotInfo, Program, Simulation}
import lince.syntax.{Lince, Show}
import SmallStep.St
import caos.frontend.widgets.WidgetInfo.Custom

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Simulation]:
  val name = "Animator of Lince 2.0"
  override val languageName: String = "Input program"

  /** Parser, converting a string into a System in Lince 2.0 */
  val parser =
    lince.syntax.Parser.parseSimulation

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
    "CC (broken)" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n  if x>=10 then x:=10;\n}\n--\nuntil 5\nsamples 10",
    "CC" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n}\n--\nuntil 5",
    "x:=2" -> "x := 2;",
    "x'=2,y'=3 for 4" -> "x'=2, y'=3 for 4;",
    "skip" -> "skip",
    "PPDP - Ex.2.6a"
      -> "// Adaptive Cruise Control (ACC)\np:=0; v:=0;  // follower  \npl:=50; vl:=10; // leader\na:=0;\nwhile true {\n  // decide to speed up (acc=2) or brake (acc=-2)\n  if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=a for 1;\n  else p'=v, v'=-2, pl'=vl, vl'=a for 1;\n}\n----\nuntil 20",

    "Single tank (poll-variation)"
      ->
      """// Define initial values of the water tank
        |level := 5;
        |drain := -1/2;
        |
        |while true do {
        |  // keep level between 3..10
        |  if      level<=3  then drain:= 1/2;
        |  else if level>=10 then drain:=-1/2;
        |  else    skip;
        |
        |	level'= drain, drain'=0
        | 	  for 0.1;
        |}
        |---
        |until 100
        |iterations 1000""".stripMargin
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
    "View parsed" -> view(_.toString,Text).moveTo(1),
    "View pretty" -> view[Simulation](s=>Show(s._1),Code("clike")).moveTo(1),
    "Plot"
      -> Custom[Simulation](divName = "sim-plotly", reload = sim => {
      val js = Plot.plotToJS(
        Plot.apply(mkSt(sim),sim._2.minTime,sim._2.maxTime,"sim-plotly", samples = sim._2.samples),
        "sim-plotly")
      scala.scalajs.js.eval(js)
    }, buttons = Nil).expand,
    "Run small-steps" -> steps[Simulation,Action,St]
      (mkSt, SmallStep, Show.simpleSt, _.toString, Text),
    "Run all steps" -> lts[Simulation,Action,St]
      (mkSt, SmallStep, Show.simpleSt, _.toString),
//    "Plot debug"
//      -> view[Simulation](sim=>
//          Plot(mkSt(sim), sim._2.minTime, sim._2.maxTime, "divName", samples = sim._2.samples).show,
//          Text),
//    "Plot JS"
//      -> view[Simulation](sim=>
//          Plot.plotToJS(
//            Plot.apply(mkSt(sim),sim._2.minTime,sim._2.maxTime,"sim-plotly", samples = sim._2.samples),
//            "sim-plotly"),
//          Text),
  )

  def mkSt(sim:Simulation): St =
    St(sim._1,Map(),sim._2.maxTime,sim._2.maxLoops)

  //// Documentation below

  override val footer: String =
    """Simple animator of Lince 2.0, meant for cyber physical systems, describing programs with discrete and continuous evolution.
      | Source code available online:
      | <a target="_blank" href="https://github.com/arcalab/lince-2.0">
      | https://github.com/arcalab/lince-2.0</a>.""".stripMargin

  override val documentation: Documentation = List(
    languageName -> "More information on the syntax of Lince 2.0" ->
      """<p>A program <code>p</code> in Lince 2.0 is given by the following grammar:
        |<pre>
        |  p ::= a  |  skip  |  p p  |  if b then p else p  |  while b {p}
        |  a ::= x1'=e, ...,xn'=e for e;  |  x:=e;
        |  e ::= x  |  f(e,...,e)
        |  b ::= e <= e  |  b && b  |  b || b  |  true  |  false
        |</pre></p>
        |<p> Known functions for <code>f</code> include <code>*</code>, <code>/</code>, <code>+</code>, <code>-</code>, <code>^</code>, <code>sqrt</code>, <code>exp</code>, <code>sin</code>, <code>cos</code>, <code>tan</code>, <code>cosh</code>, <code>sinh</code>, <code>tanh</code>, <code>pi</code>.</p>
        |<p> You can customize your plot by appending to the end of your program, e.g.,
        |<pre>
        |---
        |until 5 // maximum time
        |from 0 // starting time
        |iterations 10 // maximum times the while loops are unfolded
        |samples 40 // minumum number of points to be sampled when drawing the plot
        |</pre>
        |</p>
        |""".stripMargin,
  )
