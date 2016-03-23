package okey

case class History(
  lastMoves: List[Action] = Nil,
  openStates: Sides[Option[Opens]] = Sides[Option[Opens]]
) {

  def withLastMove(move: Action) = copy(lastMoves = List(move))

  def addLastMove(move: Action) = copy(lastMoves = move :: lastMoves)

  def withOpenStates(oopener: Option[Opener]) = oopener.fold(this) { opener =>
    copy(openStates = opener.opens map Opens.apply)
  }

  def withOpenStates(states: Sides[Option[Opens]]) = copy(openStates = states)
}

object History {
  def apply(actions: Action*): History =
    History(lastMoves = actions.toList)
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
