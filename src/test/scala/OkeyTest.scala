package okey

import okey.format.{ Visual }

import Piece._

import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import ornicar.scalalib.test.ValidationMatchers

import scalaz.{ Validation => V }

trait OkeyTest extends Specification
    with ValidationMatchers {

  def stringToDuzOkey(str: String): Table = Visual <<?(okey.variant.DuzOkey, str)

  def stringToDuzOkeySituation(str: String) = new {
    def as(player: Player): Situation = Situation(Visual <<?(okey.variant.DuzOkey, str), player)
  }

  def makeDuzOkeyGame(str: String, player: Player) = situationToGame(stringToDuzOkeySituation(str) as player)

  implicit def stringToTable(str: String): Table = Visual << str

  implicit def stringToSituationBuilder(str: String) = new {
    def as(player: Player): Situation = Situation(Visual << str, player)
  }

  implicit def piecesToSeries(pieces: List[Piece]): OpenSerie =
    StandardGrouper(R13).series(pieces) get

  implicit def piecesToPairs(pieces: List[Piece]): OpenPair =
    StandardGrouper(R13).pairs(pieces) get

  implicit def groupsToSeries(pieces: PieceGroups): List[OpenSerie] =
  StandardGrouper(R13).seriesSeq(pieces) get

  implicit def groupsToPairs(pieces: PieceGroups): List[OpenPair] =
    StandardGrouper(R13).pairsSeq(pieces) get

  implicit def richGame(game: Game) = new {
    def playMoves(side: Side, moves: Action*): Valid[Game] = playMoveList(side, moves)

    def playMoveList(side: Side, moves: Iterable[Action]): Valid[Game] = {
      val vg = moves.foldLeft(V.success(game): Valid[Game]) { (vg, move) =>
        vg flatMap { g => g(side, move) map (_._1) }
      }
      vg
    }

    def withClock(c: Clock) = game.copy(clock = Some(c))
  }

  def situationToGame(situation: Situation) = {
    Game(situation.table, situation.player)
  }

  def makeGame: Game = Game(makeTable, Player(EastSide))

  def makeTable: Table = Table init okey.variant.Standard

  def makeDuzOkey: Table = Table init okey.variant.DuzOkey

  def makeDuzOkey(side: Side): Table =
    Table init(okey.variant.DuzOkey, side)

  def makeTable(side: Side): Table =
    Table init(okey.variant.Standard, side)

  def makeTable(east: Piece, west: Piece, north: Piece, south: Piece, sign: Piece, middle: Piece, variant: okey.variant.Variant = okey.variant.Standard): Table = {
    val grouper = StandardGrouper(sign)
    Table(
      boards = Sides(
        Board(List.fill(22)(east)),
        Board(List.fill(21)(west)),
        Board(List.fill(21)(north)),
        Board(List.fill(21)(south))),
      discards = Sides(
        List(east),
        List(west),
        List(north),
        List(south)
      ),
      middles = List.fill(18)(middle),
      opener = variant.hasOpener option Opener(List(
        EastSide -> grouper.series(Piece.<>(10)).get,
        WestSide -> grouper.series(Piece.<>(11)).get,
        WestSide -> grouper.series(Piece.<>(12)).get
      ), List(
        SouthSide -> grouper.pairs(R10.w).get,
        SouthSide -> grouper.pairs(L10.w).get,
        SouthSide -> grouper.pairs(G10.w).get,
        SouthSide -> grouper.pairs(B10.w).get
      ),
        opens = Sides(
          eastSide = OldOpen(SerieScore(40)).some,
          westSide = OldOpen(SerieScore(44 + 48)).some,
          southSide = OldOpen(PairScore(4)).some,
          northSide = None)),
      sign = sign,
      variant = variant)
  }

  def makeOpener: Opener = Opener empty

  def makeBoard(pieces: Piece*): Board =
    Board(pieces)

  def makeBoard: Board = Board empty

  def containPieces(pieces: Piece*): Matcher[List[Piece]] = { l: List[Piece] =>
    l must_== pieces
  }

  def haveOpenPairs(pairs: List[Piece]*): Matcher[Table] = { t: Table =>
    t.opener must beSome.like {
      case opener =>
        (opener.pairs map(_._2.pieces) flatten) must_== pairs.flatten
    }
  }

  def haveOpenSeries(series: List[Piece]*): Matcher[Table] = { t: Table =>
    t.opener must beSome.like {
      case opener =>
        (opener.series.map(_._2.pieces).flatten) must_== series.flatten
    }
  }

  def haveAllPieces: Matcher[Table] = { t: Table =>
    (t.sign :: t.middles ::: t.boards.map(_.pieceList).toList.flatten) must contain(exactly(Piece.initial :_*))
  }

  def havePieces(pieces: Piece*): Matcher[Board] = havePieces(pieces toList)

  def havePieces(pieces: List[Piece]): Matcher[Board] = { b: Board =>
    b.pieceList must contain(exactly(pieces:_*))
  }

  def beGame(visual: String): Matcher[Valid[Game]] = beSuccess.like {
    case g => g.table.visual must_== (Visual << visual).visual
  }

  def bePoss(actions: Action*): Matcher[Situation] = { s: Situation =>
    s.actions must contain(exactly(actions:_*))
  }

  def haveLastMoves(actions: Action*): Matcher[Valid[Game]] = beSuccess.like {
    case g => g.player.history.lastMoves must contain(exactly(actions map (_.toUci) :_*))
  }

  def haveOpenStates(states: Sides[Option[Opens]]): Matcher[Valid[Game]] = beSuccess.like {
    case g => g.player.history.openStates must_== states
  }

  def haveScores(totals: Sides[Int]): Matcher[Valid[Game]] = beSuccess.like {
    case g => g.situation.endScores must beSome.like {
      case sheets => sheets map(_.total) must_== totals
    }
  }
}
