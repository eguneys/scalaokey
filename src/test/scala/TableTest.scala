package okey

class TableTest extends OkeyTest {
  val table = makeTable

  "a table" should {

    "position pieces correctly" in {
      "each player gets 21 piece" in {
        table.boards(EastSide).size must_== 22
        table.boards(WestSide).size must_== 21
        table.boards(NorthSide).size must_== 21
        table.boards(SouthSide).size must_== 21
      }

      "turn player gets an extra piece" in {
        val northTable = makeTable(NorthSide)

        northTable.boards(NorthSide).size must_== 22
        northTable.boards(EastSide).size must_== 21
        northTable.boards(WestSide).size must_== 21
        northTable.boards(SouthSide).size must_== 21
      }

      "have discards be empty" in {
        table.discards(EastSide) must beEmpty
        table.discards(WestSide) must beEmpty
        table.discards(NorthSide) must beEmpty
        table.discards(SouthSide) must beEmpty
      }

      "have opens be empty" in {
        table.opens must beSome.like {
          case (sequences, pairs) => {
            sequences must beEmpty
            pairs must beEmpty
          }
        }
      }

      "have a sign piece and an fake okey piece" in {
        (table.sign up) must_== table.fakeOkey
      }

      "rest of the pieces are in middle" in {
        // 106 - 21 * 4 + 2
        table.middles.size must_== 106 - (21 * 4 + 2)

        (table.sign :: table.middles ::: table.boards.fold(_.pieceList)) must contain(exactly(Piece.initial :_*))
      }
    }
  }
}
