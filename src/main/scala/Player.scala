package okey

case class Player(side: Side,
  drawLeft: Option[Piece] = None,
  drawMiddle: Boolean = false) {

  def apply(action: Action): Player = action match {
    case LeaveTaken => copy(drawLeft = None)
    case DrawLeft(p) => copy(drawLeft = Some(p))
    case DrawMiddle => copy(drawMiddle = true)
    case Discard(p) => nextPlayer
    case _ => this
  }

  lazy val nextPlayer: Player = Player(side.next)

  lazy val drawPiece: Boolean = drawLeft.isDefined || drawMiddle
}
