package okey

import Math.max

case class Opener(
  series: List[(Side, OpenSerie)],
  pairs: List[(Side, OpenPair)],
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

  def getSave: Option[(Board, Opener)] = opens.foldLeft(None: Option[(Board, Opener)]) {
    case (Some(a), _) => Some(a)
    case (_, Some(o: NewOpen)) => Some((o.boardSave, o.openerSave))
    case _ => None
  }

  def seqOpener(actions: Opener => Option[Opener]*): Option[Opener] =
    actions.foldLeft(Some(this): Option[Opener])(_ flatMap _)


  private def findScore(opens: List[OpenGroup]): Int =
    opens.foldLeft(0) { _ + _.score }

  def openSeries(side: Side, opened: List[OpenSerie], board: Board): Option[Opener] = {
    val score = findScore(opened)
    val openState = opens(side) match {
      case None => OpenState(
        score = SerieScore(score),
        boardSave = board,
        openerSave = this)
      case Some(o: NewOpen) => o.addScore(score)
      case Some(o: OldOpen) => o
    }

    Some(copy(
      series = series ::: opened.map((side, _)),
      opens = opens.withSide(side, Some(openState))
    ))
  }

  def openPairs(side: Side, opened: List[OpenPair], board: Board): Option[Opener] = {
    val score = findScore(opened)
    val openState = opens(side) match {
      case None => OpenState(
        score = PairScore(score),
        boardSave = board,
        openerSave = this)
      case Some(o: NewOpen) => o.addScore(score)
      case Some(o: OldOpen) => o
    }

    Some(copy(
      pairs = pairs ::: opened.map((side, _)),
      opens = opens.withSide(side, Some(openState))
    ))
  }

  def updateSeries(serie: OpenSerie, at: Int): Option[Opener] = {
    series.lift(at) map { case (side, oldSerie) =>
      val newSeries = series.updated(at, side -> serie)

      val openState = opens(side) match {
        case Some(o: NewOpen) => Some(o.addScore(serie.score - oldSerie.score))
        case x => x
      }

      copy(series = newSeries, opens = opens.withSide(side, openState))
    }
  }

  def updatePairs(pair: OpenPair, at: Int): Option[Opener] = {
    pairs.lift(at) map { oldPair =>
      val newPairs = pairs.updated(at, oldPair._1 -> pair)
      copy(pairs = newPairs)
    }
  }

  def commitOpen(side: Side): Option[Opener] = opens(side) match {
    case Some(a: NewOpen) => Some(copy(opens = opens.withSide(side, Some(a.commit))))
    case _ => None
  }

  def collectOpen(side: Side): Option[Opener] = opens(side) match {
    case Some(a: NewOpen) => Some(a.openerSave)
    case _ => None
  }

  def withSeries(series: List[(Side, OpenSerie)]) = copy(series = series)
  def withPairs(pairs: List[(Side, OpenPair)]) = copy(pairs = pairs)

}

object Opener {
  def empty = Opener(
    series = Nil,
    pairs = Nil,
    opens = Sides[Option[OpenState]])
}
