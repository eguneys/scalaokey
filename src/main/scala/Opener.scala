package okey

sealed trait Opener {
  val series: List[OpenSerie]
  val pairs: List[OpenPair]

  def seqOpener(actions: Opener => Valid[Opener]*): Valid[Opener] =
    actions.foldLeft(success(this): Valid[Opener])(_ flatMap _)

  def openSeries(side: Side, pieces: PieceGroups): Valid[Opener]
  def openPairs(side: Side, pieces: PieceGroups): Valid[Opener]
}

case class OldOpener(
  series: List[OpenSerie],
  pairs: List[OpenPair]) extends Opener {

  def openSeries(side: Side, pieces: PieceGroups): Valid[Opener] =
    success(copy(series = series ::: pieces.map { ps => OpenSerie(side, ps) }))

  def openPairs(side: Side, pieces: PieceGroups): Valid[Opener] =
    success(copy(pairs = pairs ::: pieces.map { ps => OpenPair(side, ps) }))

}

case class NewOpener(
  series: List[OpenSerie],
  pairs: List[OpenPair],
  side: Side,
  boardSave: Board,
  openerSave: OldOpener) extends Opener {

  def openSeries(side: Side, pieces: PieceGroups): Valid[Opener] =
    success(copy(series = series ::: pieces.map { ps => OpenSerie(side, ps) }))

  def openPairs(side: Side, pieces: PieceGroups): Valid[Opener] =
    success(copy(pairs = pairs ::: pieces.map { ps => OpenPair(side, ps) }))

}


object Opener {
  def empty = OldOpener(Nil, Nil)
}
