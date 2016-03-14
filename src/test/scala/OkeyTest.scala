package okey

import okey.format.{ Visual }

import Piece._

import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import ornicar.scalalib.test.ValidationMatchers

import scalaz.{ Validation => V }

trait OkeyTest extends Specification
    with ValidationMatchers {

  implicit def stringToTable(str: String): Table = Visual << str

  implicit def stringToSituationBuilder(str: String) = new {
    def as(player: Player): Situation = Situation(Visual << str, player)
  }

  implicit def richGame(game: Game) = new {
    def playMoves(side: Side, moves: Action*): Valid[Game] = playMoveList(side, moves)

    def playMoveList(side: Side, moves: Iterable[Action]): Valid[Game] = {
      val vg = moves.foldLeft(V.success(game): Valid[Game]) { (vg, move) =>
        vg flatMap { g => g(side, move) map (_._1) }
      }
      vg
    }
  }

  def situationToGame(situation: Situation) = {
    Game(situation.table, situation.player)
  }

  def makeGame: Game = Game(makeTable, Player(EastSide))

  def makeTable: Table = Table init okey.variant.Standard

  def makeTable(side: Side): Table =
    Table init(okey.variant.Standard, side)

  def makeTable(east: Piece, west: Piece, north: Piece, south: Piece, sign: Piece, middle: Piece): Table =
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
      opener = Some(Opener(List(
        OpenSerie(EastSide, Piece.<>(10)),
        OpenSerie(WestSide, Piece.<>(11)),
        OpenSerie(WestSide, Piece.<>(12))
      ), List(
        OpenPair(SouthSide, R10),
        OpenPair(SouthSide, L10),
        OpenPair(SouthSide, G10),
        OpenPair(SouthSide, B10)
      ),
        opens = Sides(
          eastSide = OldOpen(SerieScore(40)).some,
          westSide = OldOpen(SerieScore(44 + 48)).some,
          southSide = OldOpen(PairScore(4)).some,
          northSide = None))),
      sign = sign,
      variant = variant.Standard)

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
        (opener.pairs map(_.pieces) flatten) must_== pairs.flatten
    }
  }

  def haveOpenSeries(series: List[Piece]*): Matcher[Table] = { t: Table =>
    t.opener must beSome.like {
      case opener =>
        (opener.series.map(_.pieces).flatten) must_== series.flatten
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
}
