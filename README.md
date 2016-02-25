## Rules

101

Each player gets 21 pieces
Turn player gets an extra piece


* When player has no board piece when opened game can end even if the score isn't enough

#### Side

East
West
North
South

Sides

#### Board

pieces: List[Piece]

#### Open

owner: Side
pieces: List[Piece]

OpenSerie
OpenPair

#### Table

boards: Sides[Board]
discard pieces: Sides[List[Piece]]
middle pieces: List[Piece]
opens: Option[(Sequence, Pair)]
sign piece: Piece

101

Draw Left
Draw Middle
Discard Left
Discard End
Open Sequence
Open Pairs

Process piece to x [l|r|replace]
Collect Open
Leave Draw

Standard

Draw Left
Draw Middle
Discard Left
Discard End

#### Player

#### Situation
Middle End
When middle has no pieces left
Discard End
When a player board has no pieces left after player discards piece
Player Turn End
When a player discards piece

Available moves
- Draw middle piece
  - Discard a piece
  - Open series
    - Open series
    - Collect open
    - Process piece
  - Open pairs
    - Open pairs
    - Collect open
    - Process piece
- Draw left piece
  - Open series
  - Open pairs
  - Leave taken
