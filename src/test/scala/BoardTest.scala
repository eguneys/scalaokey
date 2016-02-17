package okey

import Piece._

class BoardTest extends OkeyTest {

  val emptyBoard = makeBoard
  val board = makeBoard(R10, B2, B2, G3)

  "a board" should {

    "be empty by default" in {
      emptyBoard.pieces must beEmpty
    }

    "be queried by single piece" in {
      emptyBoard(R10) must_== 0
      board(R10) must_== 1
    }

    "be queried by pieces" in {
      board.pieces must havePairs(R10 - 1, B2 - 2, G3 - 1)
    }

    "must have size" in {
      emptyBoard.size must_== 0
      board.size must_== 4
    }

    "allow a piece to be placed" in {
      emptyBoard place R10 must beSome.like {
        case b => b(R10) must_== 1
      }

      board place R10 must beSome.like {
        case b => b(R10) must_== 2
      }
    }

    "allow a piece to be taken" in {
      board take R10 must beSome.like {
        case b => b(R10) must_== 0
      }

      board take B2 must beSome.like {
        case b => b(B2) must_== 1
      }
    }

    "not allow an piece to be taken" in {
      board take G4 must beNone
    }

    "allow pieces to be taken" in {
      board take(R10, B2) must beSome.like {
        case b => b.pieces must havePairs(B2 - 1, G3 - 1)
      }
    }


    "not allow pieces to be taken" in {
      board take(R10, B2, B2, B2) must beNone
      board take(R10, B2, G4) must beNone
    }

    "allow chaining actions" in {
      emptyBoard.seq(
        _ place R10,
        _ place R10,
        _ place B2,
        _ take R10,
        _ place G2,
        _ take B2
      ) must beSome.like {
        case b => b.pieces must havePairs(R10 - 1, G2 - 1)
      }
    }

    "fail on bad actions chain" in {
      emptyBoard.seq(
        _ place R10,
        _ place B2,
        _ take R2,
        _ place R2
      ) must beNone
    }
  }
}
