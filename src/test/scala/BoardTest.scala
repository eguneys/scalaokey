package okey

class BoardTest extends OkeyTest {

  val emptyBoard = makeBoard
  val board = makeBoard(Red - 10 -> 1, Blue - 2 -> 2, Green - 3 -> 1)

  "a board" should {

    "be empty by default" in {
      emptyBoard.pieces must beEmpty
    }

    "be queried by single piece" in {
      emptyBoard(Red - 10) must_== 0
      board(Red - 10) must_== 1
    }

    "be queried by pieces" in {
      board(Red - 10, Blue - 2) must beFalse
      board(Red - 10, Blue - 2, Blue - 2) must beFalse
      board(Red - 10, Blue - 2, Blue - 2, Blue - 2) must beFalse
      board(Red - 10, Blue - 2, Blue - 2, Blue - 2, Green - 3) must beFalse
      board(Red - 10, Blue - 2, Blue - 2, Green - 3) must beTrue
    }

    "allow a piece to be placed" in {
      emptyBoard place Red - 10 must beSome.like {
        case b => b(Red - 10) must_== 1
      }

      board place Red - 10 must beSome.like {
        case b => b(Red - 10) must_== 2
      }
    }

    "allow a piece to be taken" in {
      board take Red - 10 must beSome.like {
        case b => b(Red - 10) must_== 0
      }

      board take Blue - 2 must beSome.like {
        case b => b(Blue - 2) must_== 1
      }
    }

    "not allow an piece to be taken" in {
      board take Green - 4 must beNone
    }

    "allow pieces to be taken" in {
      board take(Red - 10, Blue - 2) must beSome.like {
        case b => { b(Blue - 2, Green - 3) must beTrue }
      }
    }


    "not allow pieces to be taken" in {
      board take(Red - 10, Blue - 2, Blue - 2, Blue - 2) must beNone
      board take(Red - 10, Blue - 2, Green - 4) must beNone
    }

    "allow chaining actions" in {
      emptyBoard.seq(
        _ place Red - 10,
        _ place Red - 10,
        _ place Blue - 2,
        _ take Red - 10,
        _ place Green - 2,
        _ take Blue - 2
      ) must beSome.like {
        case b => b(Red - 10, Green - 2) must beTrue
      }
    }

    "fail on bad actions chain" in {
      emptyBoard.seq(
        _ place Red - 10,
        _ place Blue - 2,
        _ take Red - 2,
        _ place Red - 2
      ) must beNone
    }
  }
}
