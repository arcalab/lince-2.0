package lince.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import lince.backend.*
import lince.syntax.Lince.{PlotInfo, Program, Simulation}
import lince.syntax.{Lince, Show}
import Semantics.St

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Simulation]:
  val name = "Animator of Lince 2.0"
  override val languageName: String = "Input program"

  /** Parser, converting a string into a System in Lince 2.0 */
  val parser =
    lince.syntax.Parser.parseSimulation

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
    "CC" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n}\n--\nuntil 5",
    "x:=2" -> "x := 2;",
    "x'=2,y'=3 for 4" -> "x'=2, y'=3 for 4;",
    "skip" -> "skip",
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
    "View parsed" -> view(_.toString,Text).moveTo(1),
    "View pretty" -> view[Simulation](s=>Show(s._1),Code("clike")).moveTo(1),
    "Run small-steps" -> steps[Simulation,String,St]
      (mkSt, Semantics, Show.simpleSt, _.toString, Text).expand,
    "Run all steps" -> lts[Simulation,String,St]
      (mkSt, Semantics, Show.simpleSt, _.toString),
    "Plot debug"
      -> view[Simulation](sim=>Plot(mkSt(sim), "divName", samples = sim._2.samples), Text),
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
        |<p> You can customize your plot by appending to the end of your program, e.g.,
        |<pre>
        |---
        |until 5 // maximum time
        |iterations 10 // maximum times the while loops are unfolded
        |samples 40 // minumum number of points to be sampled when drawing the plot
        |</pre>
        |</p>
        |""".stripMargin,
  )
