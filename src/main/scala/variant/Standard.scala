package okey
package variant

import okey.{ ScoringSystem => AbstractScoringSystem }

case object Standard extends Variant(
  id = 1,
  name = "Standard") {

  val scoringSystem = ScoringSystem

  override def finalizeTable(table: Table, player: Player, action: Action): Table = table
}

object ScoringSystem extends AbstractScoringSystem {

  def scorer(flag: Flag): FlagScore = ???

}
