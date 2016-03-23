package okey

case class Game(
  table: Table,
  player: Player = Player(EastSide, drawMiddle = true),
  turns: Int = 0) {

  def apply(side: Side, action: Action): Valid[(Game, Move)] = situation.move(side, action) map { move =>
    apply(move) -> move
  }

  def apply(move: Move): Game = {
    val newTurns = turns + (move.action match {
      case Discard(_) => 1
      case _ => 0
    })

    val newGame = copy(
      table = move.finalizeAfter,
      player = move.finalizePlayer,
      turns = newTurns
    )
    newGame
  }

  lazy val situation = Situation(table, player)
}

object Game {

  def apply(variant: okey.variant.Variant): Game = new Game(
    table = Table init variant
  )
}
