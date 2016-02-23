package okey

case class Opener(
  series: List[OpenSerie],
  pairs: List[OpenPair],
  opens: Side[Option[OpenState]]) {

  def seqOpener(actions: Opener => Valid[Opener]*): Valid[Opener] =
    actions.foldLeft(success(this): Valid[Opener])(_ flatMap _)


  def openSeries(side: Side, pieces: PieceGroups): Valid[Opener] =
    success(copy(series = series ::: pieces.map { ps => OpenSerie(side, ps) }))

  def openPairs(side: Side, pieces: PieceGroups): Valid[Opener] =
    success(copy(pairs = pairs ::: pieces.map { ps => OpenPair(side, ps) }))

}

object Opener {
  def empty = Opener(
    series = Nil,
    pairs = Nil,
    opens = Sides[Option[OpenState]])
}
