package okey

case class Game(
  table: Table,
  player: Player) {

  def apply(side: Side, action: Action): Valid[(Game, Move)] = situation.move(side, action) map { move =>
    apply(move) -> move
  }

  def apply(move: Move): Game = {
    val newGame = copy(
      table = move.after,
      player = player(move.action)
    )
    newGame
  }

  lazy val situation = Situation(table, player)
}
