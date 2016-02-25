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
  val score: OpenScore
}

case class OldOpen(score: OpenScore) extends OpenState

case class NewOpen(
  score: OpenScore,
  boardSave: Board,
  openerSave: Opener) extends OpenState {

  def addScore(s: Int) = copy(score = score.add(s))

  def commit: OldOpen = OldOpen(score = score)
}

object OpenState {
  def apply(score: OpenScore, boardSave: Board, openerSave: Opener): NewOpen =
    NewOpen(score, boardSave, openerSave)
}

sealed trait OpenScore {
  val score: Int
  def add(score: Int): OpenScore
}

case class SerieScore(score: Int) extends OpenScore {
  def add(s: Int): SerieScore = copy(score = score + s)
}
case class PairScore(score: Int) extends OpenScore {
  def add(s: Int): PairScore = copy(score = score + s)
}
