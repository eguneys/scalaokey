package okey

sealed trait Open {
  def pieces: List[Piece]
}

case class OpenSerie(pieces: List[Piece]) extends Open
case class OpenPair(pieces: List[Piece]) extends Open
