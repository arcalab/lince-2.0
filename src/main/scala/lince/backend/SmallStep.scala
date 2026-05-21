package lince.backend

import caos.sos.SOS
import lince.backend.BasicSmallStep.BasicState
import lince.backend.ConcurrentSmallStep.ConcurrentState
import lince.syntax.Lince.*
import lince.backend.Eval.Valuation

object SmallStep extends SOS[Action, SmallStep.St]:

  enum St:
    case Basic(st: BasicState)
    case Concurrent(st: ConcurrentState)

  val defaultRKSamples: Int = 100

  override def accepting(s: St): Boolean =
    s match
      case St.Basic(st) =>
        BasicSmallStep.accepting(st)
      case St.Concurrent(st) =>
        ConcurrentSmallStep.accepting(st)

  def next[A >: Action](st: St): Set[(A, St)] =
    step(st)(using defaultRKSamples).toSet

  def step(st: St)(using rkSamples: Int): Option[(Action, St)] =
    st match
      case St.Basic(bst) =>
        BasicSmallStep.step(bst)(using rkSamples).map {
          case (a, bst2) => a -> St.Basic(bst2)
        }

      case St.Concurrent(cst) =>
        ConcurrentSmallStep.step(cst)(using rkSamples).map {
          case (a, cst2) => a -> St.Concurrent(cst2)
        }

  
  // ---- Generic state accessors used by BigSteps, Plot and Show ----

  def valuation(st: St) =
    st match
      case St.Basic(bst)      => bst.v
      case St.Concurrent(cst) => cst.v

  def time(st: St): Double =
    st match
      case St.Basic(bst)      => bst.t
      case St.Concurrent(cst) => cst.t

  def loops(st: St): Int =
    st match
      case St.Basic(bst)      => bst.lp
      case St.Concurrent(cst) => cst.lp

  def resetSeed(st: St): Unit =
    st match
      case St.Basic(bst)      => bst.resetSeed
      case St.Concurrent(cst) => cst.resetSeed

  def nextSeed(st: St): St =
    st match
      case St.Basic(bst) =>
        St.Basic(bst.nextSeed)

      case St.Concurrent(cst) =>
        St.Concurrent(cst.nextSeed)

  def currentProgram(st: St): Program =
    st match
      case St.Basic(bst) =>
        bst.p

      case St.Concurrent(cst) =>
        cst.progs.head._2

  def withTime(st: St, t: Double): St =
    st match
      case St.Basic(bst) =>
        St.Basic(bst.copy(t = t))

      case St.Concurrent(cst) =>
        St.Concurrent(cst.copy(t = t))

  def withValuation(st: St, v: Eval.Valuation): St =
    st match
      case St.Basic(bst) =>
        St.Basic(bst.copy(v = v))

      case St.Concurrent(cst) =>
        St.Concurrent(cst.copy(v = v))

  def withSeed(st: St, s: Long): St =
    st match
      case St.Basic(bst) =>
        St.Basic(bst.copy(s = s))

      case St.Concurrent(cst) =>
        St.Concurrent(cst.copy(s = s))
  
  def hasInstantaneousStep(st: St): Boolean =
    def nextStatement(p: Program): Program = p match
      case Program.Seq(Program.Seq(p1, p2), q) =>
        nextStatement(Program.Seq(p1, Program.Seq(p2, q)))
      case Program.Seq(Program.Skip, q) =>
        nextStatement(q)
      case Program.Seq(p, _) =>
        nextStatement(p)
      case _ =>
        p

    st match
      case St.Basic(bst) =>
        nextStatement(bst.p) match
          case Program.Skip       => false
          case Program.EqDiff(_, _) => false
          case _                  => true

      case St.Concurrent(cst) =>
        cst.progs.values.exists { p =>
          nextStatement(p) match
            case Program.Skip         => false
            case Program.EqDiff(_, _) => false
            case _                    => true
        }