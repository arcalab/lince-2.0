package lince.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import lince.backend.*
import lince.backend.plot.*
import lince.syntax.Lince.{Action, PlotInfo, Program, Simulation}
import lince.syntax.{Lince, Show}
import SmallStep.St
import caos.frontend.widgets.WidgetInfo.Custom

import scala.util.Random

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Simulation]:
  val name = "Animator of Lince 2.0"
  override val languageName: String = "Input program"

  /** Parser, converting a string into a System in Lince 2.0 */
  val parser =
    lince.syntax.Parser.parseSimulation

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
    "Simple composition"
      -> "p:=0; v:=0;\np'=v,v'= 2  for 5;\np'=v,v'=-2  for 5;"
      -> "Composing two trajectories: the 1st accelerates (2) and the 2nd brakes (-2).",
    "CC (broken)" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n  if x>=10 then x:=10;\n}\n--\nuntil 5\nsamples 10",
    "CC" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n}\n--\nuntil 5",
    "x:=2" -> "x := 2;",
    "skip" -> "skip;",
    "Bouncing ball"
      -> "// Bouncing ball example\nv:=5; p:=10; c:=0;\nwhile (c<4) do {\n  v'= -9.8, p'=v until_0.05 p<0 && v<0;\n  v:= -0.5*v; c:=c+1;\n}\n--\niterations 1000 "
      -> "<strong>Bouncing Ball</strong><p>Event-Driven (ED) example, using steps of 0.01. A ball position and velocity as it bounces in the floor. It includes an experimental feature: using a condition (p<0 /\\ v<0) to end a trajectory using a naive search algorithm.</p>",
    "Firefiles"
      -> "f1 := 1; f2 := 4;\nwhile true {\n  f1'=1, f2'=1 until_0.1\n       f1>10 || f2>10;\n  if f1>=10 && f2<10\n    then { f1:=0; f2:=f2+2; }\n    else if f2>=10 && f1<10\n         then { f2:=0;f1 :=f1 +2; }\n         else { f1:=0; f2 :=0; }\n}\n--\nuntil 30\niterations 1000"
      -> "<strong>Fireflies 2x</strong>\n\nEvent-Driven (ED) example. Every firefly has an internal clock that helps it to know when to flash: when the clock reaches a threshold the firefly flashes and the clock’s value is reset to zero. If other fireflies are nearby then they try to synchronise their flashes in a decentralised way.This version synchronises 2 fireflies.",
    "PPDP - Ex.2.1"
      -> "x := 0 ; c := 0 ;\nwhile c <= 5 do {\n  bernoulli (1/2)\n    x++; x--;\n  c++;\n} \nx := x/sqrt(5);"
      -> "Example 2.1 - An execution sample of a random walk with 50% chances of increasing or decreasing a variable x.",
    "PPDP - Ex.2.2"
      -> "x := 0;\nwhile true {\n  bernoulli (1/2)\n    x++; x--;\n  wait unif(0,1);\n}"
      -> "Example 2.2 - An execution sample of a continuous-time random walk in which the waiting time is given by sampling from the uniform distribution on [0,1].",
    "PPDP - Ex.2.3"
      -> "d:=0;\np := 10 ; v := 0 ;\nwhile true {\n  d := unif (2,4) ;\n  p'=v,v'= -9.8 for d ;\n  v := -v;\n}"
      -> "Example 2.3 - An execution sample of the ball's position (p) and velocity (v) during the first 5 time units.",
    "PPDP - Ex.2.4"
      -> "lambda:=2; d:=0;\n// the \"seed\" variable fixes\n// the pseudo-random generator\np:=0; v:=0; a:=0;\nwhile true {\n  d:=expn(lambda);\n  bernoulli (1/2)\n  \ta--; a++;\n  p'= v, v'=a for d;\n}\n---\nruns 20\nuntil 15\nvars p"
      -> "Example 2.4 - Multiple execution samples of the particle’s position overlayed, in order to depict how the position’s probability mass spreads over space w.r.t time.",
    "PPDP - Ex.2.5"
      -> "x:=expn(2); y:=expn(2);\np:=0; v:=0;\np'=v, v'=1  for sqrt (3) + x;\np'=v, v'=-1 for sqrt (3) + y;"
      -> "Example 2.5 - Execution sample of a particle's position (p) and velocity (v).",
    "PPDP - Ex.2.6a"
      -> "// Adaptive Cruise Control (ACC)\np:=0; v:=0;  // follower  \npl:=50; vl:=10; // leader\na:=0;\nwhile true {\n  // decide to speed up (acc=2) or brake (acc=-2)\n  if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=a for 1;\n  else p'=v, v'=-2, pl'=vl, vl'=a for 1;\n}\n----\nuntil 20\nvars p.*"
      -> "Example 2.6a - Adaptive cruise control with a leader with constant acceleration (no uncertainty).",
    "PPDP - Ex.2.6b"
      -> "// Adaptive Cruise Control (ACC)\np:=0; v:=0;  // follower  \npl:=50; vl:=10; // leader\na:=0;\nwhile true {\n\ta := unif(-1,1) ;\n  // decide to speed up (acc=2) or brake (acc=-2), assuming a==-1\n  if (v - vl + 3)^2 + 4*(p - pl + v - vl + 3/2) < 0\n  // uncomment the \"if\" below to try version 2.6a (incorrect)\n  // if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=a for 1;\n  else p'=v, v'=-2, pl'=vl, vl'=a for 1;\n}\n----\nuntil 20\nseed 10\nvars p.*"
      -> "Example 2.6b Adaptive cruise control with a leader with an uncertain acceleration (bounded by fixed values).",
    "PPDP - Ex.2.6c"
      -> "// Adaptive Cruise Control (ACC)\n// (unexpected delay)\np := 0; v := 0; // follower  \npl:=50; vl:=10; // leader\nlambda:=2;\nwhile true {\n  x := expn(lambda) ; x++ ;\n  // decide to speed up (acc=2) or brake (acc=-2)\n  if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=0 for x;\n  else p'=v, v'=-2, pl'=vl, vl'=0 for x;\n}\n----\nuntil 20\nvars p.*"
      -> "Example 2.6c: Adaptive cruise control with a leader with an uncertain delay during rounds (unbounded, given by a exponential distribution).",
    "Single tank (poll-variation)"
      -> "// Define initial values of the water tank\nlevel := 5;\ndrain := -1/2;\n\nwhile true do {\n  // keep level between 3..10\n  if      level<=3  then drain:= 1/2;\n  else if level>=10 then drain:=-1/2;\n  else    skip;\n\n\tlevel'= drain, drain'=0\n \t  for 0.1;\n}\n---\nuntil 100\niterations 1000",
    "Water tank (slow)"
      -> "a1:=1; //Area of tank 1\na2:=1; // Area of tank 2\nr1:=1; //Resistance applied to the water flow at the water exit tap of tank 1.\nr2:=10; //Resistance applied to the water flow at the water exit tap of tank 2.\nh1_p:=10; // initial water level of tank 1 in the aligned  configuration\nh2_p:=0; // initial water level of tank 2 in the aligned  configuration.\nh1_v:=10; //initial water level of tank 1 in vertical configuration.\nh2_v:=0; //initial water level of tank 2 in vertical  configuration.\n\n// Open tap of the tank 1 and close the tap of the tank 2\nqe1:=1;\nqe2:=0;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n// Open tap of the tank 2 and close the tap of the tank 1\nqe1:=0;\nqe2:=1;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n//Open both\nqe1:=1;\nqe2:=1;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n//Close both\nqe1:=0;\nqe2:=0;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n---\nuntil\t100\nsamples 60\nvars h.*"
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
    "View parsed" -> view(_.toString,Text).moveTo(1),
    "View pretty" -> view[Simulation](s=>Show(s._1),Code("clike")).moveTo(1),
    "Plots"
      -> Custom[Simulation](divName = "sim-plotlys", reload = sim => {
          val plots = Plot.allPlots(sim.state, sim.pi)
          val js = PlotToJS(plots.head._1, "sim-plotlys", plots.head._2) + "\n" +
                   plots.tail.map(p=>PlotToJS.addPlot(p._1, "sim-plotlys", p._2)).mkString("\n")
          scala.scalajs.js.eval(js)
        }, buttons = Nil).expand,
    "Run small-steps" -> steps[Simulation,Action,St]
      (_.state, SmallStep, Show.simpleSt, _.toString, Text),
    "Run all steps" -> lts[Simulation,Action,St]
      (_.state, SmallStep, Show.simpleSt, _.toString),
    // "Run all steps (inf)" -> lts[Simulation,Action,St]
    //   (_.state, StillSmallStep, Show.simpleSt, _.toString),
    "Final state" -> view[Simulation](sim => Show.simpleSt(BigSteps.bigStep(sim.state,Nil)._2),Text),
    "Plot debug"
      -> view[Simulation](sim=>
          Plot(sim.state, sim._2).show,
          Text),
    "Plots JS"
      -> view(sim => {
            val plots = Plot.allPlots(sim.state, sim.pi)
            PlotToJS(plots.head._1, "sim-plotlys", plots.head._2) + "\n" +
              plots.tail.map(p=>PlotToJS.addPlot(p._1, "sim-plotlys", p._2)).mkString("\n")
          }, Text),

//    "Plot"
//      -> Custom[Simulation](divName = "sim-plotly", reload = sim => {
//          val js = PlotToJS(Plot(sim.state, sim.pi), "sim-plotly", sim.pi)
//          scala.scalajs.js.eval(js)
//        }, buttons = Nil).expand,
//    "Plot JS"
//      -> view[Simulation](sim=>
//          PlotToJS(Plot(sim.state, sim._2), "sim-plotly", sim.pi),
//          Text),
  )


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
        |  p ::= a  |  skip  |  p p  |  if b [then] p else p  |  while b p  |  { p }
        |  a ::= x1'=e, ...,xn'=e for e;  |  x:=e;
        |  e ::= x  |  f(e,...,e)
        |  b ::= e <= e  |  b && b  |  b || b  |  true  |  false
        |</pre></p>
        |<p> Known functions for <code>f</code> include <code>*</code>, <code>/</code>, <code>+</code>, <code>-</code>, <code>^</code>, <code>pow</code>, <code>sqrt</code>, <code>exp</code>, <code>sin</code>, <code>cos</code>, <code>tan</code>, <code>cosh</code>, <code>sinh</code>, <code>tanh</code>, <code>pi</code>, <code>unif</code>, <code>expn</code>, <code>powerlaw</code>.</p>
        |<p> You can customize your plot by appending to the end of your program, e.g.,
        |<pre>
        |---
        |until 5 // maximum time (default 10)
        |from 0 // starting time (default 0)
        |iterations 10 // maximum times the while loops are unfolded (default 50)
        |samples 40 // minumum number of points to be sampled when drawing the plot (default 20)
        |seed 0 // seed for the random generator  (every time a random one by default)
        |vars x.*, y // list of regular expressions to select variables to be displayed (default all)
        |height 450 // sets the height in px of the graph (default 450)
        |runs 5 // number of plots to draw (default 1, useful for random plots)
        |verbose // shows a marker at every discrete step (does not show by default)
        |</pre>
        |</p>
        |<h3>Note on stochastic functions</h3>
        |<p> The functions below are stochastic, and will yield a different value every time they are called. For reproducibility, you can set the "seed" value to fix the internal pseudo-random generator.
          <ul>
            <li><code>unif(a,b)</code> - random value from a uniform distribution between <code>a</code> and <code>b</code>;</li>
            <li><code>unif()</code> - equivalent to <code>unif(0,1)</code></li>
            <li><code>expn(lambda)</code> - random variable from a negative exponential distribution, using an average frequence of <code>lambda</code> (equivalent to <code>-ln(unif()) / lambda</code>);</li>
            <li><code>powerlaw(alpha,xmin)</code> - random variable from a power law distribution, using a normalisation value <code>alpha > 1</code> and a lower bound <code>xmin</code> for the returned values (equivalent to <code>xmin * unif()^(−1/(alpha−1))</code>).</li>
          </ul>
        |</p>
        |""".stripMargin,
    "Run small-steps" -> "Information on the semantics rules used by Lince" ->
      "<p>The operational rules, borrowed from a PPDP'25 publication, can be found below.</p><img src=\"img/lince-semantics.svg\" style: width=50%;s/>"
//      "<object class=\"pdf\" data=\"img/lince-semantics.pdf\" width=\"800\" height=\"500\"></object>",
  )
