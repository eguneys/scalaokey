package okey

import format.Uci

case class History(
  lastMoves: List[Uci] = Nil,
  turns: Int,
  openStates: Sides[Option[Opens]] = Sides[Option[Opens]]
) {

  def hasEverybodyPlayed = turns > 3

  def addTurns = copy(turns = turns + 1)

  def withLastMove(move: Action) = copy(lastMoves = List(move.toUci))

  def addLastMove(move: Action) = copy(lastMoves =
    lastMoves :+ move.toUci)

  def withOpenStates(oopener: Option[Opener]) = oopener.fold(this) { opener =>
    copy(openStates = opener.opens map Opens.apply)
  }

  def withOpenStates(states: Sides[Option[Opens]]) = copy(openStates = states)
}

object History {
  def apply(actions: Action*): History =
    History(lastMoves = actions.toList map(_.toUci), turns = 0)
}

case class Opens(pairs: Boolean, old: Boolean)

case object Opens {
  def apply(state: Option[OpenState]): Option[Opens] = state map {
    case NewOpen(SerieScore(_), _, _) => Opens(false, false)
    case NewOpen(PairScore(_), _, _) => Opens(true, false)
    case OldOpen(SerieScore(_)) => Opens(false, true)
    case OldOpen(PairScore(_)) => Opens(true, true)
  }
}
