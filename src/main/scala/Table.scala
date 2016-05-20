package okey

import variant.{ Variant }

case class Table(
  boards: Sides[Board],
  discards: Sides[List[Piece]] = Sides[List[Piece]],
  middles: List[Piece],
  opener: Option[Opener],
  sign: Piece,
  variant: Variant,
  scoreSheet: Sides[ScoreSheet] = Sides(_ => ScoreSheet.emptySheet)) {

  import implicitFailures._

  lazy val okey: Piece = sign.up

  def actorOf(player: Player): Actor = Actor(player, this)

  def seqTable(actions: Table => Valid[Table]*): Valid[Table] =
    actions.foldLeft(success(this): Valid[Table])(_ flatMap _)

  def drawMiddle(side: Side): Valid[Table] = (for {
    p <- middles.headOption
    b1 <- boards(side) place(p)
  } yield {
    copy(middles = middles.tail, boards = boards.withSide(side, b1))
  }) toValid "Cannot draw from empty middle"

  def drawLeft(side: Side): Valid[Table] = {
    val dside = side.previous
      (for {
        discard <- discards(dside).headOption
        d1 = discards(dside).tail
        b1 <- boards(side) place discard
      } yield {
        copy(boards = boards.withSide(side, b1),
          discards = discards.withSide(dside, d1))
      }) toValid "No piece on discards " + dside
  }

  def discard(side: Side, piece: Piece): Valid[Table] = (for {
    b1 <- boards(side) take piece
    d1 = piece :: discards(side)
  } yield { copy(boards = boards.withSide(side, b1),
    discards = discards.withSide(side, d1))
  }) toValid "No piece on board " + piece


  def discardEndToValid(side: Side, groups: List[Piece]): Valid[Table] = (for {
    b1 <- boards(side) take groups
    if b1.size == 1
  } yield copy(boards.withSide(side, b1))) toValid "Invalid pieces to discard end"


  def openSeries(side: Side, groups: List[OpenSerie]): Valid[Table] = (for {
    b1 <- boards(side) take (groups.map(_.pieces).flatten)
    o1 <- opener
    o2 <- o1.openSeries(side, groups, boards(side))
  } yield {
    copy(boards = boards.withSide(side, b1), opener = Some(o2))
  }) toValid "No piece group on board " + groups


  def openPairs(side: Side, groups: List[OpenPair]): Valid[Table] = (for {
    b1 <- boards(side) take (groups.map(_.pieces).flatten)
    o1 <- opener
    o2 <- o1.openPairs(side, groups, boards(side))
  } yield {
    copy(boards = boards.withSide(side, b1), opener = Some(o2))
  }) toValid "No piece group on board " + groups

  def collectOpen(side: Side): Valid[Table] = (for {
    o1 <- opener
    o2 <- o1.collectOpen(side)
    b1 <- o1.boardSave(side)
  } yield {
    copy(boards = boards.withSide(side, b1), opener = Some(o2))
  }) toValid "No opener on table"

  def leaveTaken(side: Side, piece: Piece): Valid[Table] = {
    val dside = side.previous
    (for {
      o1 <- opener
      o2 <- o1.collectOpen(side) orElse o1.some
      b1 <- o1.boardSave(side) orElse boards(side).some
      b2 <- b1 take piece
      d1 = piece :: discards(dside)
    } yield {
      copy(boards = boards.withSide(side, b2),
        discards = discards.withSide(dside, d1),
        opener = Some(o2))
    }) toValid "No piece on board " + piece
  }

  def showSign(side: Side, piece: Piece): Valid[Table] = boards(side).exists(piece) && (piece == sign) option this toValid "No piece on board " + piece

  def dropSeries(side: Side, piece: Piece, updated: OpenSerie, at: OpenPos): Valid[Table] = (for {
    o1 <- opener toValid "No opener on table"
    b1 <- boards(side) take piece toValid s"No $piece to drop"
    b2 <- b1 place(okey) toValid s"unknown"
    o2 = o1.updateSeries(updated, at.group)
  } yield {
    val replaceBoard = at match {
      case _:ReplaceOkey => b2
      case _ => b1
    }
    copy(boards = boards.withSide(side, replaceBoard), opener = o2)
  })

  def dropPairs(side: Side, piece: Piece, updated: OpenPair, at: OpenPos): Valid[Table] = (for {
    o1 <- opener toValid "No opener on table"
    b1 <- boards(side) take piece toValid s"No $piece to drop"
    b2 <- b1 place(okey) toValid s"unknown"
    o2 = o1.updatePairs(updated, at.group)
  } yield {
    copy(boards = boards.withSide(side, b2), opener = o2)
  })

  def handSum(side: Side): Int = boards(side).pieceList.foldLeft(0) {
    case (acc, p) if p == okey => acc
    case (acc, Piece.F1) => acc + okey.number
    case (acc, p) => acc + p.number
  }

  def toDrawMiddle(side: Side): Option[Piece] = middles.headOption

  def toDrawLeft(side: Side): Option[Piece] = discards(side previous).headOption

  def toCollectOpen(side: Side): Option[(Board, Opener)] = opener.flatMap (_.getSave)

  def toOpenSerie(at: OpenPos): Option[OpenSerie] = opener flatMap (_.series.lift(at.group) map (_._2))

  def toOpenPair(at: OpenPos): Option[OpenPair] = opener flatMap (_.pairs.lift(at.group) map (_._2))

  def opens(side: Side): Option[OpenState] = opener flatMap (_.opens(side))

  // def hasOpenedSeries(side: Side): Boolean =  !(opener ?? { _.seriesOf(side) isEmpty })

  // def hasOpenedPairs(side: Side): Boolean =  !(opener ?? { _.pairsOf(side) isEmpty })

  def hasOpener: Boolean = opener.isDefined

  def hasOpenedSeries(side: Side): Boolean = opener ?? {
    _.score(side) match {
      case Some(_:SerieScore) => true
      case _ => false
    }
  }

  def hasOpenedPairs(side: Side): Boolean = opener ?? {
    _.score(side) match {
      case Some(_:PairScore) => true
      case _ => false
    }
  }

  def updateOpener(f: Opener => Opener) = copy(opener = opener map f)

  def visual = format.Visual >> this

  override def toString = List(
    visual
  ) mkString "\n"
}

object Table {
  def init(variant: Variant, player: Side = EastSide): Table = {
    val dealer = variant.dealer(player)
    Table(
      boards = dealer.boards,
      middles = dealer.middles,
      sign = dealer.sign,
      opener = variant.hasOpener option (Opener empty),
      variant = variant
    )
  }
}
