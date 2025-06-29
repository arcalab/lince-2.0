package lince.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import lince.backend.*
import lince.syntax.Lince.Program
import lince.syntax.{Lince, Show}

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Program]:
  val name = "Animator of Lince 2.0"
  override val languageName: String = "Input program"

  /** Parser, converting a string into a System in Lince 2.0 */
  val parser =
    lince.syntax.Parser.parseProgram

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
    "CC" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n}",
    "x:=2" -> "x := 2;",
    "x'=2,y'=3 for 4" -> "x'=2, y'=3 for 4;",
    "skip" -> "skip",
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
    "View parsed" -> view(_.toString,Text).expand,
    "View pretty" -> view[Program](Show.apply,Text),
    "Run semantics" -> steps[Program,String,Semantics.St]
      (p=>(p,Map(),3,100), Semantics, Show.simpleSt, _.toString, Text).expand,
    "Plot debug: up to time 5, 10 samples "
      -> view[Program](p=>Plot((p,Map(),5,100), "divName", samples = 10), Text),
  )

  //// Documentation below

  override val footer: String =
    """Simple animator of Lince 2.0, meant for cyber physical systems, describing programs with discrete and continuous evolution.
      | Source code available online:
      | <a target="_blank" href="https://github.com/arcalab/lince-2.0">
      | https://github.com/arcalab/lince-2.0</a>.""".stripMargin

  override val documentation: Documentation = List(
    languageName -> "More information on the syntax of Lince 2.0" ->
      """A program <code>p</code> in Lince 2.0 is given by the following grammar:
        |<pre>
        |  p ::= a  |  skip  |  p p  |  if b then p else p  |  while b {p}
        |  a ::= x1'=e, ...,xn'=e for e;  |  x:=e;
        |  e ::= x  |  f(e,...,e)
        |  b ::= e <= e  |  b && b  |  b || b  |  true  |  false
        |</pre>
        |""".stripMargin,
  )
