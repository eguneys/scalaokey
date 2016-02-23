package okey
package format

/**
  * r13
  * r2r2r2r2r2r2r2r2r2r2r2r2r2r2r2r2r2r2
  * r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1
  * l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1
  * g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1
  * b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1
  * r1
  * l1
  * g1
  * b1
  * r10l10g10b10 r11l11g11b11 r12l12g12b12 r13l13g13b13
  * er10r10 wl10l10 ng10g10 sb10b10
  * 
  */

/*
 * sign
 * middles
 * board east
 * board west
 * board north
 * board south
 * discard east
 * discard west
 * discard north
 * discard south
 * opens sequence
 * opens pairs
 */

object Visual {

  val SideR = """(e|w|n|s)""".r

  val PieceR = """(r|l|g|b)([1-9][0-3]?)""".r

  def parseOwnedPieces(str: String): (Side, List[Piece]) = {
    val side = str(0) match {
      case SideR(s) => Side(s)
    }

    val pieces = parsePieces(str.tail)

    (side, pieces)
  }

  def parsePieces(str: String): List[Piece] = {
    PieceR.findAllIn(str).toList map {
      case PieceR(c, n) => Piece(Color(c(0)) get, n.toInt)
    }
  }

  def trimList(l: Array[String]): List[String] = l match {
    case Array("") => Nil
    case l => l toList
  }

  def <<(source: String): Table = {
    val lines = source.lines.toList drop 1
    val filtered = lines.size match {
      case 12 => lines
      case n if n > 12 => lines take 12
      case n => lines ::: (List.fill(12 - n)(""))
    }

    val opens = filtered drop 10 take 2 match {
      case series :: pairs :: Nil =>
        (trimList(series.split(" ")).map(parseOwnedPieces).map { case (side, pieces) =>
          OpenSerie(side, pieces) } toList,
          trimList(pairs.split(" ")).map(parseOwnedPieces).map { case (side, pieces) =>
            OpenPair(side, pieces) } toList
        )
      case _ => throw new Exception("Invalid visual format " + source)
    }

    filtered.take(10) map parsePieces match {
      case List(sign) :: middles :: east :: west :: north :: south ::
          deast :: dwest :: dnorth :: dsouth :: Nil =>
        Table(
          boards = Sides(east, west, north, south).map(Board.apply),
          discards = Sides(deast, dwest, dnorth, dsouth),
          sign = sign,
          middles = middles,
          opener = Some(Opener empty),
          variant = okey.variant.Standard
        )
      case _ => throw new Exception("Invalid visual format " + source)
    }
  }

  def >>(table: Table): String = {
    val boards = table.boards.map(_.pieceList mkString).fold(List(_)) mkString "\n"
    val discards = table.discards.map(_ mkString).fold(List(_)) mkString "\n"
    val sign = table.sign.toString
    val middles = table.middles mkString

    // val opens = table.opens.fold("") {
    //   case (series, pairs) =>
    //     (series map(s => s.owner.letter + s.pieces.mkString) mkString " ") + "\n" +
    //     (pairs map(p => p.owner.letter + p.pieces.mkString) mkString " ")
    // }

    List(sign, middles, boards, discards) mkString "\n"
  }

  def addNewLines(str: String) = "\n" + str + "\n"
}
