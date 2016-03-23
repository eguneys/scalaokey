package okey

case class Player(side: Side,
  history: History,
  drawMiddle: Boolean,
  drawLeft: Option[Piece]) {

  def apply(action: Action): Player = action match {
    case DrawMiddle(p) => copy(drawMiddle = true)
    case LeaveTaken => copy(drawLeft = None)
    case DrawLeft(p) => copy(drawLeft = Some(p))
    case Discard(p) => nextPlayer
    case _ => this
  }

  lazy val nextPlayer: Player = Player(side.next).withHistory(history)

  lazy val drawPiece: Boolean = drawLeft.isDefined || drawMiddle

  def withHistory(h: History) = copy(history = h)

  def updateHistory(f: History => History) = copy(history = f(history))
}

object Player {
  def apply(side: Side, drawMiddle: Boolean = false, drawLeft: Option[Piece] = None): Player =
    Player(side, History(), drawMiddle, drawLeft)
}
