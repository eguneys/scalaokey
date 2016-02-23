package okey

case class Situation(table: Table, player: Player) {

  lazy val actor = table.actorOf(player)

  lazy val moves: List[Action] = table.variant.validMoves(this)

  def middleEnd: Boolean = table.middles.isEmpty

  def discardEnd: Boolean = table.boards(player.side).isEmpty && player.discardPiece.isDefined

  def turnEnd: Boolean = player.discardPiece.isDefined
}
