package okey

class ScoreTest extends OkeyTest {

  "scoring system" should {
    val system = new ScoringSystem {
      def scorer(flag: Flag): FlagScore = ???
    }

    "find flags" in {

      "end by hand" in {
        val situation = ("""
r13
""" as Player(EastSide))

        system.flags(situation, EastSide) must_== Nil
      }
    }
  }

}
