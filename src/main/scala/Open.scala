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
  val series: Boolean
  val pairs: Boolean
  val score: Int
}

case class OldOpen(series: Boolean, pairs: Boolean, score: Int) extends OpenState

case class NewOpen(
  series: Boolean,
  pairs: Boolean,
  score: Int,
  boardSave: Board,
  openerSave: Opener) extends OpenState {

  def addScore(s: Int) = copy(score = score + s)

  def commit: OldOpen = OldOpen(series = series, pairs = pairs, score = score)
}

object OpenState {
  def series(score: Int, boardSave: Board, openerSave: Opener): NewOpen =
    NewOpen(true, false, score, boardSave, openerSave)

  def pairs(score: Int, boardSave: Board, openerSave: Opener): NewOpen =
    NewOpen(false, true, score, boardSave, openerSave)
}
