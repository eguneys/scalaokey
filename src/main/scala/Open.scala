package okey

sealed trait Open {
  val owner: Side
  val pieces: List[Piece]

  def by(side: Side): Boolean = owner == side
}

case class OpenSerie(owner: Side, pieces: List[Piece]) extends Open
case class OpenPair(owner: Side, pieces: List[Piece]) extends Open

case object OpenPair {
  def apply(side: Side, piece: Piece): OpenPair = OpenPair(side, List(piece, piece))
}
