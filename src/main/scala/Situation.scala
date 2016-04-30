package okey

case class Situation(table: Table, player: Player) {

  lazy val actor = table.actorOf(player)

  lazy val actions: List[Action] = table.variant.validMoves(this)

  def toMove(action: Action): Option[Move] = {
    actor.toMove(action)
  }

  lazy val okey = table.okey

  lazy val lastSide = player.side.previous
  lazy val lastMoves = player.history.lastMoves map(_.action)
  lazy val lastDiscard = lastMoves collectFirst { case Discard(p) => p }

  lazy val openStates = player.history.openStates
  lazy val lastSideOpens = openStates(lastSide)
  lazy val opensSize = openStates.flatten.toList length

  def middleEnd: Boolean = table.middles.isEmpty

  def normalEnd: Boolean = lastDiscard.isDefined && table.boards(lastSide).isEmpty

  def handEnd: Boolean = normalEnd && lastSideOpens.exists (!_.old) && opensSize == 1

  def pairEnd: Boolean = normalEnd && lastSideOpens.exists (_.pairs)

  def okeyEnd: Boolean = normalEnd && lastDiscard.exists (_ == okey)

  def end: Boolean = middleEnd || normalEnd

  def endScores: Option[Sides[EndScoreSheet]] = table.variant.endScores(this)

  def winner: Option[Side] = table.variant.winner(this)

  def status: Option[Status] =
    if (normalEnd) Status.NormalEnd.some
    else if (middleEnd) Status.MiddleEnd.some
    else none

  def move(side: Side, action: Action): Valid[Move] =
    table.variant.move(this, side, action)

  def withHistory(history: History) = copy(
    player = player withHistory(history)
  )
}
