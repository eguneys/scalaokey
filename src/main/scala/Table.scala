package okey

import variant.{ Variant, Dealer }

case class Table(
  boards: Sides[Board],
  discards: Sides[List[Piece]] = Sides[List[Piece]],
  middles: List[Piece],
  opens: Option[(OpenSeries, OpenPairs)] = Some((List.empty[OpenSerie], List.empty[OpenPair])),
  sign: Piece) {

  import implicitFailures._

  lazy val okey: Piece = sign.up

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


  def openSeries(side: Side, pieces: List[List[Piece]]): Valid[Table] = (for {
    b1 <- boards(side) take (pieces flatten)
    (series, pairs) <- opens
    os = pieces map (l => OpenSerie(side, l))
    s1 = series ::: os
  } yield {
    copy(boards = boards.withSide(side, b1),
      opens = Some((s1, pairs)))
  }) toValid "No piece on board " + pieces


  def openPairs(side: Side, pieces: List[List[Piece]]): Valid[Table] = (for {
    b1 <- boards(side) take (pieces flatten)
    (series, pairs) <- opens
    opairs = pieces map (l => OpenPair(side, l))
    p1 = pairs ::: opairs
  } yield {
    copy(boards = boards.withSide(side, b1),
      opens = Some((series, p1)))
  }) toValid "No piece on board " + pieces

  def collectOpen(side: Side): Valid[Table] = (for {
    (series, pairs) <- opens
    (sideSeries, s1) = series partition(_.by(side))
    (sidePairs, p1) = pairs partition(_.by(side))
    pieces = sideSeries.map(_.pieces) ::: sidePairs.map(_.pieces)
    b1 <- boards(side) place pieces.flatten
  } yield {
    copy(boards = boards.withSide(side, b1), opens = Some((s1, p1)))
  }) toValid "Cannot open on table"

  def leaveDrawn(side: Side, piece: Piece): Valid[Table] = {
    val dside = side.previous
    (for {
      b1 <- boards(side) take piece
      d1 = piece :: discards(dside)
    } yield {
      copy(boards = boards.withSide(side, b1),
        discards = discards.withSide(dside, d1))
    }) toValid "No piece on board " + piece
  }
}

object Table {
  def init(variant: Variant, player: Side = EastSide): Table = {
    val dealer = Dealer(player)
    Table(
      boards = dealer.boards,
      middles = dealer.middles,
      sign = dealer.sign
    )
  }
}
