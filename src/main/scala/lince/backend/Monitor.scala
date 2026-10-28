package lince.backend

// import Monitor.Driver
import SmallStep.St as SSt
import lince.syntax.Lince.*
import Driver.* 
import STL.*
import lince.syntax.Show

class Monitor(b: MonBuilder, si:Simulation):
  val stl = b.stl
  val drv: Driver = b.sch match
    case MonSched.SmallSteps => Driver.SmallStepDriver(si.pi.rkSamples)
    case MonSched.BigSteps   => Driver.BigStepDriver(si.pi.rkSamples)
    case MonSched.Periodic(p, err) => Driver.TimeStepDriver(p,si.pi.rkSamples)

  // we may want to optimise this later. Some repetition so far
  def check: Option[Boolean] =
    check(stl,drv.initial(si))

  def check(phi:STL, st:St): Option[Boolean] = phi match
    case Cond(e) =>
      scala.util.Try(Eval.asBoolean(e)(using st.v)).toOption
      // val r = Some(Eval.asBoolean(e)(using st.v))
      // println(s"Checking cond: ${Show(e)} @ ${st.t} --> $r") 
      // r
      
    case U(i,s1,s2) => checkUntil(i,s1,s2,st)
    case E(i,s) => check(U(i,Cond(Expr.True),s),st)
    case G(i,s) => check(Not(U(i,Cond(Expr.True),Not(s))),st)
    case And(s1,s2) => // tri-logic
      val cs1 = check(s1,st)
      if cs1.contains(false) then cs1 else
      if cs1.isEmpty then None else check(s2,st)
    case Or(s1,s2) =>
      val cs1 = check(s1,st)
      if cs1.contains(true) then cs1 else
      if cs1.isEmpty then None else check(s2,st)
    case Imply(s1,s2) =>
      check(Or(Not(s1),s2),st)
    case Not(s) => check(s,st).map(!_)
  
  def checkUntil(i:Intr,s1:STL,s2:STL,st:St): Option[Boolean] =
   val r = i match
    case (fr,_) if fr>0 => checkNext(i,s1,s2,st) // not yet
    case (_,Some(to)) if to<0 => Some(false) // reached the end, no s2 found
    case (fr,to) => check(s2,st) match
      case Some(true) => Some(true)
      case None => None
      case Some(false) => check(s1,st) match
        case Some(true) => checkNext(i,s1,s2,st)
        case Some(false) => Some(false)
        case None => None
  //  println(s"Checking until: ${Show(U(i,s1,s2))} @ ${st.t}} --> $r")
   r 
   
  def checkNext(i:Intr,s1:STL,s2:STL,st:St): Option[Boolean] = drv.step(st) match
    case None => None
    case Some((_,st2)) =>
      val delta = st2.t - st.t
      val i2 = (i._1-delta, i._2.map(_-delta))
      checkUntil(i2,s1,s2,st2)
  
  def checkAll: List[(Double,Option[Boolean])] =
    checkAll(Some(drv.initial(si)))
  def checkAll(st:Option[St]): List[(Double,Option[Boolean])] = st match
    case None => Nil
    case Some(s) =>
      (s.t -> check(stl,s)) :: checkAll(drv.step(s).map(_._2))


object Monitor:

  def monToGroupedTrPP(n:String, b: MonBuilder, si:Simulation): String =
    val grp = monToGroupedTr(b,si)
    s"Signal $n: ${grp.map(x => s"[${x._1._1},${x._1._2}] ${okPP(x._2)}").mkString("; ")}"

  def monToMerm(n:String, b: MonBuilder, si:Simulation): String =
    val grp = monToGroupedTr(b,si)
    s"section $n\n" +
    grp.map(blockPP).mkString("\n")

  def okPP(r: Option[Boolean]): String = r match
    case Some(true) => "✅"
    case Some(false) => "❌"
    case None => "?"

  private def blockPP(block:((Double,Double),Option[Boolean])): String =
    block._2 match
        case r@None        => s"  ${okPP(r)} : done,, ${block._1._1.round}, ${block._1._2-block._1._1}ms"
        case r@Some(true)  => s"  ${okPP(r)} : marketing,${block._1._1.round}, ${block._1._2-block._1._1}ms "
        case r@Some(false) => s"  ${okPP(r)} : crit,, ${block._1._1.round}, ${block._1._2-block._1._1}ms"

  lazy val mermHeader = """gantt
    dateFormat X
    axisFormat %Q
"""


  lazy val mermHeaderNew = """---
displayMode: compact
config:
    theme: forest
    themeCSS: " // YAML supports multiline strings using a newline markers: \n
        // Custom marker workaround CSS from forum (below)    \n
        rect[id^=workaround] { height: calc(100% - 50px) ; transform: translate(9px, 25px); y: 0; width: 4.5px; stroke: none; fill: red; }   \n
        text[id^=workaround] { fill: red; y: 100%; font-size: 25px;}
    "
    gantt:
        useWidth: 400
        rightPadding: 0
        topAxis: false
        numberSectionStyles: 1
    #    title Monitors
---
gantt
    dateFormat X
    axisFormat %Q
"""

  def monToGroupedTr(b: MonBuilder, si:Simulation)
      : List[((Double,Double),Option[Boolean])] =
    val tr = Monitor(b,si).checkAll
    if tr.isEmpty then Nil else
      val fst = tr.head
      group(tr.tail,(fst._1,fst._1)->fst._2)

  def group(tr:List[(Double,Option[Boolean])],
            last: ((Double,Double),Option[Boolean]))
      : List[((Double,Double),Option[Boolean])] =
    tr match
      case Nil => List(last)
      case ((d,o))::tl if o==last._2 =>
        group(tl,((last._1._1,d),o))
      case ((d,o))::tl =>
        last :: group(tl, ((d,d),o))
    

