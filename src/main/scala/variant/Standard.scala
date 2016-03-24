package okey
package variant

import okey.{ ScoringSystem => AbstractScoringSystem }

case object Standard extends Variant(
  id = 1,
  name = "Standard") {

  val scoringSystem = StandardScoringSystem

  override def finalizeTable(table: Table, player: Player, action: Action): Table = table
}

object StandardScoringSystem extends AbstractScoringSystem {

  import AbstractScoringSystem._


  case class Adder(val value: Int, flag: Flag) extends FlagScore(1)
  case class Double(flag: Flag) extends FlagScore(2)

  case class Sheet(val handSum: Int, scores: List[FlagScore]) extends EndScoreSheet {
    // val total = scores.foldRight(handSum) { _ apply _ }

    val handSumPenalty = scores.exists(_.flag == HandOpenNone).fold(0, {
      scores collectFirst {
        case Adder(value, HandOkey) => handSum + value
      } getOrElse handSum
    })

    val total = {
      val filteredScores = scores.filterNot(_.flag == HandOkey)

      val (sum, mult) = filteredScores.foldRight((handSumPenalty, 1): (Int, Int)) {
        case (_:Double, (sum, mult)) => (sum, mult * 2)
        case (Adder(value, _), (sum, mult)) => (sum + value, mult)
      }
      sum * mult
    }
  }

  def scorer(flag: Flag): FlagScore = flag match {
    case EndByHand => Double(EndByHand)
    case EndByPair => Double(EndByPair)
    case EndByDiscardOkey => Double(EndByDiscardOkey)
    case HandZero => Adder(-101, HandZero)
    case HandOkey => Adder(101, HandOkey)
    case HandOpenPair => Double(HandOpenPair)
    case HandOpenNone => Adder(101, HandOpenNone)
  }

  def sheet(situation: Situation, side: Side): Sheet = {
    val sum = handSum(situation, side)
    val scores = flags(situation, side) map { f => scorer(f) }

    Sheet(sum, scores)
  }
}
