package okey

trait Score {
  def value: Int
}

case class ScoreSheet(scores: List[Score]) {
  val total = scores.foldLeft(0)(_ + _.value)
}

object ScoreSheet {
  val emptySheet = ScoreSheet(Nil)
}

trait EndScoreSheet {
  val handSum: Int
  val scores: List[FlagScore]
  val total: Int
}

sealed abstract class Flag(val id: Int)

abstract class FlagScore(val id: Int) {
  val flag: Flag
}

abstract class ScoringSystem {

  import ScoringSystem._

  def scorer(flag: Flag): FlagScore

  def flags(situation: Situation, side: Side): List[Flag] = allFlags filter {
    case EndByHand => situation.handEnd
    case EndByPair => situation.pairEnd
    case EndByDiscardOkey => situation.okeyEnd
    case HandZero => situation.table.boards(side).isEmpty
    case HandOkey => situation.table.boards(side).exists(situation.table.okey)
    case HandOpenPair => situation.openStates(side) exists (_.pairs)
    case HandOpenNone => !situation.openStates(side).isDefined
  }

  def handSum(situation: Situation, side: Side): Int = situation.table.handSum(side)

  def sheet(situation: Situation, side: Side): EndScoreSheet
}

object ScoringSystem {

  case object EndByHand extends Flag(1)
  case object EndByPair extends Flag(2)
  case object EndByDiscardOkey extends Flag(3)

  case object HandZero extends Flag(4)
  case object HandOkey extends Flag(5)
  case object HandOpenNone extends Flag(6)
  case object HandOpenPair extends Flag(7)

  val allFlags = List(EndByHand, EndByPair, EndByDiscardOkey, HandZero, HandOkey, HandOpenNone, HandOpenPair)
}
