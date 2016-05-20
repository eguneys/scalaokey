package okey

case class Actor(player: Player, table: Table) {

  import Function.const
  import implicitFailures._

  def toMove(action: Action): Option[Move] = {
    if (!moves.contains(action.toSingle))
      None
    else
      (action match {
        case DrawMiddle =>
          table.toDrawMiddle(side) flatMap { p =>
            move(DrawMiddle(p)) { table.drawMiddle(side) }
          }
        case DrawLeft =>
          table.toDrawLeft(side) flatMap { p =>
            move(DrawLeft(p)) { table.drawLeft(side) }
          }
        case LeaveTaken =>
          move(action) {
            player.drawLeft toValid "Not drawn left" flatMap (table.leaveTaken(side, _))
          }
        case ShowSign(piece) =>
          move(action) {
            table.showSign(side, piece)
          }
        case OpenSeries(pieces) =>
          grouper.seriesSeq(pieces) flatMap { series =>
            move(action) { table.openSeries(side, series) }
          }
        case OpenPairs(pieces) =>
          grouper.pairsSeq(pieces) flatMap { pairs =>
            move(action) { table.openPairs(side, pairs) }
          }
        case CollectOpen =>
          table.toCollectOpen(side) flatMap { save =>
            move(CollectOpen(save)) { table.collectOpen(side) }
          }
        case Discard(piece) =>
          move(action) { table.discard(side, piece) }
        case DiscardEndPairs(pieces) =>
          duzGrouper.pairsSeq(pieces) flatMap { pairs =>
            move(action) { table.discardEndToValid(side, pairs flatMap (_.pieces)) }
          }
        case DiscardEndSeries(pieces) =>
          duzGrouper.seriesSeq(pieces) flatMap { series =>
            move(action) { table.discardEndToValid(side, series flatMap (_.pieces)) }
          }
        case DropOpenSeries(piece, pos) =>
          table.toOpenSerie(pos) flatMap { serie =>
            grouper.dropSeries(serie, piece, pos) flatMap { updated =>
              move(action) {
                table.dropSeries(side, piece, updated, pos)
              }
            }
          }
        case DropOpenPairs(piece, pos) =>
          table.toOpenPair(pos) flatMap { pair =>
            grouper.dropPairs(pair, piece) flatMap { updated =>
              move(action) {
                table.dropPairs(side, piece, updated, pos)
              }
            }
          }
        case _ => None
      })
  }

  lazy val grouper: Grouper = StandardGrouper(table.sign)

  lazy val duzGrouper: Grouper = StandardGrouper(table.sign, withTore = true)

  lazy val moves: List[Action] = player.drawPiece fold(afterDraw, draw)

  def draw: List[Action] = List(drawMiddle, drawLeft, showSign) flatten

  def drawMiddle: Option[Action] = table.drawMiddle(side).toOption map const(DrawMiddle)
  def drawLeft: Option[Action] = table.drawLeft(side).toOption map const(DrawLeft)

  def showSign: Option[Action] = !table.hasOpener && !player.history.hasEverybodyPlayed option ShowSign

  def afterDraw: List[Action] = table.hasOpener.fold(
    afterDrawWithOpener,
    afterDrawNoOpener)

  def afterDrawWithOpener: List[Action] = player.drawMiddle.fold(
    afterDrawMiddle,
    afterDrawLeft
  )

  def afterDrawNoOpener: List[Action] = discardEnds ::: (List(showSign, discardMiddle) flatten)

  def afterDrawMiddle: List[Action] = List(discardMiddle, openSeries, openPairs, collectOpen, dropSeries, dropPairs) flatten

  def afterDrawLeft: List[Action] = List(discardLeft, openSeries, openPairs, collectOpen, leaveTaken, dropSeries, dropPairs) flatten

  def discardLeft: Option[Action] = discard(true)
  def discardMiddle: Option[Action] = discard(false)

  def discardEnds: List[Action] = List(DiscardEndSeries, DiscardEndPairs)

  def discard(withLeft: Boolean): Option[Action] = table.opens(side) match {
    case Some(NewOpen(SerieScore(score), _, _)) if (score > minValidOpenSeriesScore) => Discard.some
    case Some(NewOpen(PairScore(score), _, _)) if (score > minValidOpenPairsScore) => Discard.some
    case Some(_:OldOpen) => Discard.some
    case None => withLeft.fold(None, Discard.some)
    case _ => None
  }

  def openSeries: Option[Action] = (!hasOpenedPairs).option(OpenSeries)
  def openPairs: Option[Action] = (!hasOpenedSeries).option(OpenPairs)

  def collectOpen: Option[Action] = table.opens(side) match {
    case Some(_:NewOpen)=> CollectOpen.some
    case _ => None
  }

  def leaveTaken: Option[Action] = table.opens(side) match {
    case Some(_:NewOpen) => LeaveTaken.some
    case Some(_:OldOpen) => None
    case None => LeaveTaken.some
  }

  def dropSeries: Option[Action] = hasOpened.option(DropOpenSeries)
  def dropPairs: Option[Action] = hasOpened.option(DropOpenPairs)

  lazy val minValidOpenSeriesScore: Int = table.opener flatMap (_.maxOpenSerieScore) getOrElse 100

  lazy val minValidOpenPairsScore: Int = table.opener flatMap (_.maxOpenPairScore) getOrElse 4

  lazy val side = player.side

  lazy val hasOpenedPairs = table.hasOpenedPairs(side)
  lazy val hasOpenedSeries = table.hasOpenedSeries(side)

  lazy val hasOpened = hasOpenedPairs || hasOpenedSeries

  def is(s: Side) = s == player.side

  private def move(action: Action)(vafter: Valid[Table]): Option[Move] = vafter map(move(action, _)) toOption

  private def move(
    action: Action,
    after: Table) = Move(
      player = player,
      action = action,
      before = table,
      after = after)
}
