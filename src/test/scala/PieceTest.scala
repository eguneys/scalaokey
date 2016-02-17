package okey

class PieceTest extends OkeyTest {
  "Piece" should {
    "compare" in {
      "objects and - method" in {
        Red - 10 must_== Red - 10
        Black - 13 must_== Black - 13
        Green - 1 must_== Green - 1
        Blue - 2 must_== Blue - 2
      }
    }
  }
}
