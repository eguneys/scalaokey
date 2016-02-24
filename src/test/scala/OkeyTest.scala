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

  // implicit def richGame(game: Game) = new {
  //   def playMoves(moves: Action*): Valid[Game] = playMoveList(moves)

  //   def playMoveList(moves: Iterable[Action]): Valid[Game] = {
  //     val vg = moves.foldLeft(V.success(game): Valid[Game]) { (vg, move) =>
  //       vg flatMap { g => g(move) }
  //     }
  //     vg
  //   }
  // }

  def makeGame: Game = Game(makeTable)

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
        OpenSerie(NorthSide, Piece.<>(12)),
        OpenSerie(SouthSide, Piece.<>(13))
      ), List(
        OpenPair(EastSide, R10),
        OpenPair(EastSide, L10),
        OpenPair(EastSide, G10),
        OpenPair(EastSide, B10)
      ),
        Sides[Option[OpenState]])),
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
    (t.sign :: t.middles ::: t.boards.fold(_.pieceList)) must contain(exactly(Piece.initial :_*))
  }

  def havePieces(pieces: Piece*): Matcher[Board] = havePieces(pieces toList)

  def havePieces(pieces: List[Piece]): Matcher[Board] = { b: Board =>
    b.pieceList must contain(exactly(pieces:_*))
  }

  def bePoss(actions: Action*): Matcher[Situation] = { s: Situation =>
    s.moves must contain(exactly(actions:_*))
  }
}
