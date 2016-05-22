package okey
package variant

import okey.{ ScoringSystem => AbstractScoringSystem, EndScoreSheet => AbstractEndScoreSheet }

case object Standard extends Variant(
  id = 1,
  key = "yuzbir",
  name = "101 Okey",
  shortName ="101",
  title = "101'i geç, elini aç, en düşük puanı al, kazan.") {

  val scoringSystem = StandardScoringSystem

  override def finalizeTable(table: Table, player: Player, action: Action): Table = table
}

case object StandardTest extends Variant(
  id = 2,
  key = "yuzbirtest",
  name = "StandardTest",
  shortName ="StdTest",
  title = "Standard Test rules of okey") {

  val scoringSystem = StandardScoringSystem

  override def dealer(side: Side) = TestDealer(side)
}

object StandardScoringSystem extends AbstractScoringSystem {

  import AbstractScoringSystem._
  import FlagScore._

  // case class Sheet(val handSum: Int, scores: List[FlagScore]) extends EndScoreSheet {
  //   // val total = scores.foldRight(handSum) { _ apply _ }

  //   val handSumPenalty = scores.exists(_.flag == HandOpenNone).fold(0, {
  //     scores collectFirst {
  //       case Adder(value, HandOkey) => handSum + value
  //     } getOrElse handSum
  //   })

  //   val total = {
  //     val filteredScores = scores.filterNot(_.flag == HandOkey)

  //     val (sum, mult) = filteredScores.foldRight((handSumPenalty, 1): (Int, Int)) {
  //       case (_:Double, (sum, mult)) => (sum, mult * 2)
  //       case (Adder(value, _), (sum, mult)) => (sum + value, mult)
  //     }
  //     sum * mult
  //   }
  // }

  case class EndScoreSheet(handSum: Int, scores: Map[Flag, Option[FlagScore]]) extends AbstractEndScoreSheet {

    val total: Int = {
      val (sum, mult) = scores.foldRight((0, 1)) {
        case ((f, Some(HandSum)), (s, m)) => (s + handSum, m)
        case ((f, Some(Penalty)), (s, m)) => (s + 101, m)
        case ((f, Some(Erase)), (s, m)) => (s - 101, m)
        case ((f, Some(Double)), (s, m)) => (s, m * 2)
        case (_, (s, m)) => (s, m)
      }
      sum * mult
    }
  }

  def sheet(situation: Situation, side: Side): EndScoreSheet = {
    val scoresValue = scores(situation, side)
    val handSumValue = handSum(situation, side)

    EndScoreSheet(handSumValue, scoresValue)
  }

  def scorer(flag: Flag, flags: List[Flag]): Option[FlagScore] = flag match {
    case EndByHand => Double.some
    case EndByPair => Double.some
    case EndByDiscardOkey => Double.some
    case HandZero => Erase.some
    case HandOkey => flags.find(_ == HandOpenSome).map(_ => Penalty)
    case HandOpenPair => Double.some
    case HandOpenNone => Penalty.some
    case HandOpenSome => HandSum.some
  }
}
