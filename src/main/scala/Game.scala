package okey

case class Game(
  table: Table,
  player: Player = Player(EastSide, drawMiddle = true),
  clock: Option[Clock] = None,
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
      turns = newTurns,
      clock = applyClock(move)
    )

    newGame
  }

  private def applyClock(move: Move) = clock map {
    case c: RunningClock =>
      move.action match {
        case _:Discard => c.step.emptyTime(player.side)
        case _ => c.stepPly
      }
    case c: PausedClock if (turns == 0) => c.start.switch
    case c => c.switch
  }

  lazy val situation = Situation(table, player)
}

object Game {

  def apply(variant: okey.variant.Variant): Game = new Game(
    table = Table init variant
  )
}
