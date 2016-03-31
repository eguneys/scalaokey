package okey

object Grouper
    extends scalaz.std.OptionInstances
    with scalaz.syntax.ToTraverseOps {


  private def findScore(pieces: List[Piece]): Int = pieces.foldLeft(0) { _ + _.number }

  def series(pieces: List[Piece]): Option[OpenSerie] = OpenSerie(pieces, findScore(pieces)).some
  def pairs(pieces: List[Piece]): Option[OpenPair] = OpenPair(pieces, 1).some

  def seriesSeq(pieces: PieceGroups): Option[List[OpenSerie]] =
    pieces.map(group => series(group)).sequence

  def pairsSeq(pieces: PieceGroups): Option[List[OpenPair]] =
    pieces.map(group => pairs(group)).sequence
}
