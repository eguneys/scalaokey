package okey

case class Situation(table: Table, player: Player) {

  lazy val actor = table.actorOf(player)

  lazy val actions: List[Action] = table.variant.validMoves(this)

  def toMove(action: Action): Option[Move] = {
    actor.toMove(action)
  }

  def middleEnd: Boolean = table.middles.isEmpty

  def discardEnd: Boolean = table.boards(player.side).isEmpty

  def turnEnd: Boolean = false

  def move(side: Side, action: Action): Valid[Move] =
    table.variant.move(this, side, action)
}
