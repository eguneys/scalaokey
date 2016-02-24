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
  val score: Int
}

case class OldOpen(score: Int) extends OpenState

case class NewOpen(
  score: Int,
  boardSave: Board,
  openerSave: Opener) extends OpenState {

  def addScore(s: Int) = copy(score = score + s)

  def commit: OldOpen = OldOpen(score = score)
}

object OpenState {
  def apply(score: Int, boardSave: Board, openerSave: Opener): NewOpen =
    NewOpen(score, boardSave, openerSave)
}
