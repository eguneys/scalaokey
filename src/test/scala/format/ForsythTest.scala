package okey
package format

import Piece._

class ForsythTest extends OkeyTest {

  val f = Forsyth

  "new game" in {
    val table = makeTable(
      east = R1,
      west = L2,
      north = G3,
      south = B4,
      sign = R5,
      middle = L6)
    val game = Game(table, Player(EastSide))
    val east = "r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1"
    val west = "l2l2l2l2l2l2l2l2l2l2l2l2l2l2l2l2l2l2l2l2l2"
    val north = "g3g3g3g3g3g3g3g3g3g3g3g3g3g3g3g3g3g3g3g3g3"
    val south = "b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4"
    val discards = "r1 l2 g3 b4"
    val series = "r10l10g10b10 r11l11g11b11 r12l12g12b12"
    val pairs = "r10r10 l10l10 g10g10 b10b10"
    val middle = "18r5"

    val rest = List(discards, series, pairs, middle) mkString "/"

    "opening" in {
      f >> (game, EastSide) must_== east + "/" + rest
      f >> (game, WestSide) must_== west + "/" + rest
      f >> (game, NorthSide) must_== north + "/" + rest
      f >> (game, SouthSide) must_== south + "/" + rest
    }

    "one discard" in {
      val east = "l6r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1"
      val discards = "r1r1 l2 g3 b4"
      val middle = "17r5"

      game.playMoves(EastSide, DrawMiddle, Discard(R1)) must beSuccess.like {
        case g => f >> (g, EastSide) must_== (List(east, discards, series, pairs, middle) mkString "/")
      }
    }
  }
}
