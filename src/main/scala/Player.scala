package okey

case class Player(side: Side,
  drawLeft: Boolean = false,
  drawMiddle: Boolean = false,
  discardPiece: Option[Discard] = None) {

  def drawPiece: Boolean = drawLeft || drawMiddle
}
