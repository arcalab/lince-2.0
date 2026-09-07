package lince.backend

import caos.sos.SOS
import lince.backend.BasicSmallStep.BasicState
import lince.backend.ConcurrentSmallStep.ConcurrentState
import lince.backend.Eval.Valuation
import lince.backend.Stream.Streams

import lince.syntax.Lince.*
import Program.*

/**
 * Dispatcher for Lince small-step semantics.
 *
 * A simulation can either contain:
 *   - a single program, handled by BasicSmallStep
 *   - multiple programs, handled by ConcurrentSmallStep
 */
object SmallStep extends SOS[Action, SmallStep.St]:

  enum St:
    case Basic(st: BasicState)
    case Concurrent(st: ConcurrentState)

  val defaultRKSamples: Int = 100

  override def accepting(st: St): Boolean =
    st match
      case St.Basic(bst) =>
        BasicSmallStep.accepting(bst)

      case St.Concurrent(cst) =>
        ConcurrentSmallStep.accepting(cst)

  /**
   * Set of possible next transitions.
   *
   * Both underlying semantics are deterministic, so this set
   * contains at most one transition.
   */
  def next[A >: Action](st: St): Set[(A, St)] =
    step(st)(using defaultRKSamples).toSet

  /**
   * Perform one small step by dispatching to the appropriate
   * semantics.
   */
  def step(
      st: St
  )(using rkSamples: Int): Option[(Action, St)] =

    st match

      case St.Basic(bst) =>
        BasicSmallStep
          .step(bst)(using rkSamples)
          .map { case (action, bst2) =>
            action -> St.Basic(bst2)
          }

      case St.Concurrent(cst) =>
        ConcurrentSmallStep
          .step(cst)(using rkSamples)
          .map { case (action, cst2) =>
            action -> St.Concurrent(cst2)
          }


  // ------------------------------------------------------------
  // Common state accessors
  // ------------------------------------------------------------

  def valuation(st: St): Valuation =
    st match
      case St.Basic(bst) =>
        bst.v

      case St.Concurrent(cst) =>
        cst.v

  def streams(st: St): Streams =
    st match
      case St.Basic(bst) =>
        bst.s

      case St.Concurrent(cst) =>
        cst.s

  def time(st: St): Double =
    st match
      case St.Basic(bst) =>
        bst.t

      case St.Concurrent(cst) =>
        cst.t

  def loops(st: St): Int =
    st match
      case St.Basic(bst) =>
        bst.lp

      case St.Concurrent(cst) =>
        cst.lp


  // ------------------------------------------------------------
  // Common state updates
  // ------------------------------------------------------------

  def withTime(
      st: St,
      t: Double
  ): St =

    st match
      case St.Basic(bst) =>
        St.Basic(
          bst.copy(t = t)
        )

      case St.Concurrent(cst) =>
        St.Concurrent(
          cst.copy(t = t)
        )


  // ------------------------------------------------------------
  // Program inspection
  // ------------------------------------------------------------

  /**
   * Returns one representative program.
   *
   * For a basic state this is simply the current program.
   * For a concurrent state this should only be used for
   * presentation/debugging; semantic decisions should inspect
   * all programs instead.
   */
  def currentProgram(st: St): Program =
    st match

      case St.Basic(bst) =>
        bst.p

      case St.Concurrent(cst) =>
        processOrder(cst)
          .flatMap(cst.progs.get)
          .headOption
          .getOrElse(Skip)


  /**
   * True when there is an instantaneous step available.
   *
   * This is useful for BigSteps: it must exhaust discrete
   * transitions before asking the semantics to evolve time.
   */
  def hasInstantaneousStep(st: St): Boolean =
    st match

      case St.Basic(bst) =>
        nextStatement(bst.p) match
          case Skip =>
            false

          case EqDiff(_, _) =>
            false

          case _ =>
            true

      case St.Concurrent(cst) =>
        cst.progs.values.exists { p =>
          nextStatement(p) match
            case Skip =>
              false

            case EqDiff(_, _) =>
              false

            case _ =>
              true
        }


  // ------------------------------------------------------------
  // Program helpers
  // ------------------------------------------------------------

  private def nextStatement(p: Program): Program =
    p match
      case Skip =>
        Skip

      case Seq(Skip, q) =>
        nextStatement(q)

      case Seq(p, _) =>
        nextStatement(p)

      case _ =>
        p

  private def processOrder(
      st: ConcurrentState
  ): List[String] =
    st.progs.keys.toList.sortBy { name =>
      if name == "" then " "
      else name
    }