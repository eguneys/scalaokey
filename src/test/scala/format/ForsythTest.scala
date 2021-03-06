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

    val rest = List(discards, series, pairs) mkString "/"

    "opening" in {
      f >> (game, EastSide) must_== List("e", middle, east, rest).mkString("/")
      f >> (game, WestSide) must_== List("w", middle, west, rest).mkString("/")
      f >> (game, NorthSide) must_== List("n", middle, north, rest).mkString("/")
      f >> (game, SouthSide) must_== List("s", middle, south, rest).mkString("/")
    }

    "one discard" in {
      val east = "l6r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1"
      val discards = "r1r1 l2 g3 b4"
      val middle = "17r5"

      game.playMoves(EastSide, DrawMiddle, Discard(R1)) must beSuccess.like {
        case g => f >> (g, EastSide) must_== (List("e", middle, east, discards, series, pairs) mkString "/")
      }
    }
  }

  "discards" should {

    "no discard" in {
      val table = """
r1
r1r2r3
"""

      f.exportTable(table, EastSide) must_== "e/3r1//   //"
    }

    "one discard" in {
      val table = """
r1
r1r2r3




r1
"""

      f.exportTable(table, EastSide) must_== "e/3r1//r1   //"

      f.exportTable("""
r1
r1r2r3





r1r2
""", EastSide) must_== "e/3r1// r1r2  //"
    }
  }

  "with fake" should {
    val table = ("""
r13
f1r1g1
f1g2g4



f1g1



ef1g1l1
wf1f1
""")

    f.exportTable(table, EastSide) must_== "e/3r13/f1g2g4/f1g1   /f1g1l1/f1f1"
  }
}
