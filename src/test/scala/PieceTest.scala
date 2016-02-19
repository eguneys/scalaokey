package okey

import Piece._

class PieceTest extends OkeyTest {
  "Piece" should {
    "compare" in {
      "objects and - method" in {
        R10 must_== Piece(Red, 10)
        L13 must_== Piece(Black, 13)
        G1 must_== Piece(Green, 1)
        B2 must_== Piece(Blue, 2)

        R10 - 1 must_== R10 -> 1
      }
      "fake" in {
        F1.color must_== Color.Fake
      }
    }

    "sizes" in {
      Piece.initial.size must_== 106
    }

    "direction" in {
      R11.up must_== R12
      R12.up must_== R13
      R13.up must_== R1
      G1.up must_== G2
    }

    "be used to derive relative pieces" in {
      "Piece.<>" in { Piece.<>(4) must contain(exactly(R4, L4, G4, B4)) }
      "R4 |> 3" in { R4 |> 3 must contain(exactly(R4, R5, R6)) }
      "R7 |> 6" in { R8 |> 6 must contain(exactly(R8, R9, R10, R11, R12, R13)) }
      "R10 |> 6" in { R10 |> 6 must contain(exactly(R10, R11, R12, R13)) }
      "R4 <| 4" in {  R4 <| 4 must contain(exactly(R4, R3, R2, R1)) }
      "R4 <| 5" in {  R4 <| 4 must contain(exactly(R4, R3, R2, R1)) }

    }
  }
}
