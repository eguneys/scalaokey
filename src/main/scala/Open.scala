package okey

sealed trait OpenGroup {
  val pieces: List[Piece]
  val score: Int
}

case class OpenSerie(pieces: List[Piece], score: Int) extends OpenGroup
case class OpenPair(pieces: List[Piece], score: Int) extends OpenGroup

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

sealed trait OpenPos {
  val group: Int
  val key: String

  override def toString = key + group
}

case class AppendLeft(group: Int) extends OpenPos {
  val key = "l"
}
case class AppendRight(group: Int) extends OpenPos {
  val key = "r"
}
case class ReplaceOkey(group: Int) extends OpenPos {
  val key = "p"
}

object OpenPos {

  val PosR = """(l|r|p)(\d\d?)""".r

  def apply(str: String): Option[OpenPos] = str match {
    case PosR("l", group) => AppendLeft(group.toInt).some
    case PosR("r", group) => AppendRight(group.toInt).some
    case PosR("p", group) => ReplaceOkey(group.toInt).some
    case _ => None
  }
}
