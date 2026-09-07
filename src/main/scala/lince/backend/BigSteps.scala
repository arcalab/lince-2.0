package lince.backend

import lince.backend.Eval.Valuation
import lince.backend.SmallStep.{St, step}
import lince.syntax.Lince.*
import lince.syntax.Lince.Program.*
import lince.syntax.Show

import scala.annotation.tailrec

/**
 * Collection of functions that perform more than one small step at a time.
 * This includes just discrete steps, just continuous steps, or until the end.
 */

object BigSteps:

  /** Performs all steps until no more step can be taken. */
  def bigStep(st: St, hist: List[Action] = Nil)(using rkSamples: Int): (List[Action], St) =
    step(st)(using rkSamples) match
      case Some((act, st2)) => bigStep(st2, act :: hist)
      case None => hist -> st

  /**
   * Checks the next statement in a sequence of statements, and jumping over Skips.
   * @param p Program to inspect
   * @return the next statement of the program `p`
   */
  def nextStatement(p: Program): Program = p match {
    case Seq(Seq(p1, p2), q) => nextStatement(Seq(p1, Seq(p2, q)))
    case Seq(Skip, q) => nextStatement(q)
    case Seq(p, q) => nextStatement(p)
    case _ => p
  }

  def nextStatementRest(p: Program, rest:Program = Skip): (Program,Program) = p match {
    case Seq(Seq(p1, p2), q) => nextStatementRest(Seq(p1, Seq(p2, q)), rest)
    case Seq(Skip, q) => nextStatementRest(q,rest)
    case Seq(p, q) => nextStatementRest(p,Seq(q,rest))
    case _ => (p,rest)
  }

  def currentProgram(st: St): Program =
    SmallStep.currentProgram(st)

  def contProgram(st: St): Program =
    currentProgram(st)
  
  /**
   * Performs discrete steps until no more step can be taken
   * @param st initial state
   * @param hist history of actions taken
   * @return Pair with the list of actions taken and the reached state
   */
  @tailrec
  def discSteps(st: St, hist: List[Action] = Nil)(using rkSamples: Int): (List[Action], St) =
    if !SmallStep.hasInstantaneousStep(st) then
      hist -> st
    else
      step(st)(using rkSamples) match
        case None =>
          hist -> st
        case Some((a, st2)) =>
          discSteps(st2, a :: hist)


  /**
   * Traverses a state of the small-step semantics iteratively just considering continuous steps, until either:
   *   - no more continuous step can be taken, or
   *   - the time bound is reached.
   * Compiles a sequence of valuations along the path, sampled every `timeStep`.
   * @param st state to be traversed
   * @param timeStep time interval used to sample values while traversing the state
   * @param baseTime time passed at the beginning of the traversal (to be added)
   * @param counter accumulator to know how many values were sampled
   * @param hist accumulator to compile the valuations of points already sampled
   * @return list of valuations at the points sampled while traversing the continuous step
   */
  def contSteps(st: St, timeStep: Double, baseTime: Double )(using rkSamples: Int): (List[(Double, Valuation)], St) =
      @tailrec
      def contStepsAux(
          counter: Int,
          hist: List[(Double, Valuation)]
      )(using rkSamples: Int): (List[(Double, Valuation)], St) =
        val remainingTime =
          SmallStep.time(st)
        val goalTime =
          remainingTime min (timeStep * counter)
        val limitedState =
          SmallStep.withTime(st, goalTime)
        step(limitedState)(using rkSamples) match
          case Some((Action.DiffStop(_, _), st2)) =>
            if goalTime == remainingTime then
              val point =
                (
                  baseTime + goalTime,
                  SmallStep.valuation(st2)
                )
              (point :: hist) -> st2
            else
              contStepsAux(
                counter + 1,
                (
                  (
                    baseTime + goalTime,
                    SmallStep.valuation(st2)
                  ) :: hist
                )
              )
          case Some((Action.DiffSkip(_, timePassed), st2)) =>
            val restored =
              SmallStep.withTime(
                st2,
                remainingTime - timePassed
              )
            val point =
              (
                baseTime + timePassed,
                SmallStep.valuation(restored)
              )
            (point :: hist) -> restored
          case Some((stp, _)) =>
            sys.error(
              s"Expected continuous step but found ${Show(stp)}"
            )
          case None =>
            hist -> st
      contStepsAux(1, Nil)

