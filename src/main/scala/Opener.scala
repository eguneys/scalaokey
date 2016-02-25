package okey

import Math.max

case class Opener(
  series: List[OpenSerie],
  pairs: List[OpenPair],
  opens: Sides[Option[OpenState]]) {

  import implicitFailures._

  lazy val maxOpenSerieScore: Option[Int] = opens.foldLeft(None: Option[Int]) {
    case (None, Some(OldOpen(SerieScore(n)))) => Some(n)
    case (Some(acc), Some(OldOpen(SerieScore(n)))) => Some(max(acc, n))
    case (acc, _) => acc
  }

  lazy val maxOpenPairScore: Option[Int] = opens.foldLeft(None: Option[Int]) {
    case (None, Some(OldOpen(PairScore(n)))) => Some(n)
    case (Some(acc), Some(OldOpen(PairScore(n)))) => Some(max(acc, n))
    case (acc, _) => acc
  }

  def score(side: Side): Option[OpenScore] = opens(side) map(_.score)

  def boardSave(side: Side): Option[Board] = opens(side) match {
    case Some(o: NewOpen) => Some(o.boardSave)
    case _ => None
  }

  def seqOpener(actions: Opener => Option[Opener]*): Option[Opener] =
    actions.foldLeft(Some(this): Option[Opener])(_ flatMap _)


  private def findScore(pieces: List[Piece]): Int =
    pieces.foldLeft(0) { _ + _.number }

  def openSeries(side: Side, pieces: PieceGroups, board: Board): Option[Opener] = {
    val openState = opens(side) match {
      case None => OpenState(
        score = SerieScore(findScore(pieces flatten)),
        boardSave = board,
        openerSave = this)
      case Some(o: NewOpen) => o.addScore(findScore(pieces flatten))
      case Some(o: OldOpen) => o
    }

    Some(copy(
      series = series ::: pieces.map { ps => OpenSerie(side, ps) },
      opens = opens.withSide(side, Some(openState))
    ))
  }

  def openPairs(side: Side, pieces: PieceGroups, board: Board): Option[Opener] = {
    val openState = opens(side) match {
      case None => OpenState(
        score = PairScore(pieces.length),
        boardSave = board,
        openerSave = this)
      case Some(o: NewOpen) => o.addScore(pieces.length)
      case Some(o: OldOpen) => o
    }

    Some(copy(
      pairs = pairs ::: pieces.map { ps => OpenPair(side, ps) },
      opens = opens.withSide(side, Some(openState))
    ))
  }

  def commitOpen(side: Side): Option[Opener] = opens(side) match {
    case Some(a: NewOpen) => Some(copy(opens = opens.withSide(side, Some(a.commit))))
    case _ => None
  }

  def collectOpen(side: Side): Option[Opener] = opens(side) match {
    case Some(a: NewOpen) => Some(a.openerSave)
    case _ => None
  }

  def seriesOf(side: Side): List[OpenSerie] = series filter (_.owner == side)
  def pairsOf(side: Side): List[OpenPair] = pairs filter (_.owner == side)

  def withSeries(series: List[OpenSerie]) = copy(series = series)
  def withPairs(pairs: List[OpenPair]) = copy(pairs = pairs)

}

object Opener {
  def empty = Opener(
    series = Nil,
    pairs = Nil,
    opens = Sides[Option[OpenState]])
}
