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

  def scorer(flag: Flag): FlagScore = flag match {
    case EndByHand => Double(EndByHand)
    case EndByPair => Double(EndByPair)
    case EndByDiscardOkey => Double(EndByDiscardOkey)
    case HandZero => Adder(-101, HandZero)
    case HandOkey => Adder(101, HandOkey)
    case HandOpenPair => Double(HandOpenPair)
    case HandOpenNone => Adder(101, HandOpenNone)
  }

}
