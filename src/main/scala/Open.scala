package okey

sealed trait OpenGroup {
  val owner: Side
  val pieces: List[Piece]

  def by(side: Side): Boolean = owner == side
}

case class OpenSerie(owner: Side, pieces: List[Piece]) extends OpenGroup
case class OpenPair(owner: Side, pieces: List[Piece]) extends OpenGroup

case object OpenPair {
  def apply(side: Side, piece: Piece): OpenPair = OpenPair(side, List(piece, piece))
}

sealed trait OpenState {

}

case class OldOpen extends OpenState

case class NewOpen(
  boardSave: Board,
  openerSave: Opener) extends OpenState
