## Rules

101

Each player gets 21 pieces
Turn player gets an extra piece


## Actions


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
player: Side

101

Draw Left
Draw Middle
Discard Left
Discard End
Open Sequence
Open Pairs

Standard

Draw Left
Draw Middle
Discard Left
Discard End
