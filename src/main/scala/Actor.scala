package okey

case class Actor(player: Player, table: Table) {

  import Function.const

  lazy val moves: List[Action] = player.drawPiece fold(afterDraw, draw)

  def draw: List[Action] = List(drawMiddle, drawLeft) flatten

  def drawMiddle: Option[Action] = table.drawMiddle(player.side).toOption map const(DrawMiddle)
  def drawLeft: Option[Action] = table.drawLeft(player.side).toOption map const(DrawLeft)

  def afterDraw: List[Action] = table.boards(player.side).isEmpty.fold(
    Nil,
    player.discardPiece.isDefined.fold(
      Nil,
      player.drawMiddle.fold(
        afterDrawMiddle,
        afterDrawLeft
      )
    )
  )

  def afterDrawMiddle: List[Action] = List(discard, openSeries, openPairs) flatten

  def afterDrawLeft: List[Action] = List(openSeries, openPairs, leaveTaken) flatten

  def discard: Option[Action] = Discard.some

  def openSeries: Option[Action] = OpenSeries.some
  def openPairs: Option[Action] = OpenPairs.some

  def leaveTaken: Option[Action] = LeaveTaken.some
}
