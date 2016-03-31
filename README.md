## Rules

101

Each player gets 21 pieces
Turn player gets an extra piece


* When player opens and has no board piece game, can end even if the score isn't enough, but hadn't processed
* When player draws left, drawn piece must be used on opens

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


#### Move

Draw Middle (piece)
Discard (piece)
Draw Left
Leave taken
open series (piece groups)
collect open
process (piece)

### FEN

specific for side

middle = middleCount + gosterge

board/discards/series/pairs/middle


#### Scores

If you have single piece left and discard, game ends

##### End by Hand

When nobody opened, you open and end, you end by hand, 
Opponents get double score

##### End by Pair

If you opened pairs, and end, you end by pair
Opponents get double score

##### End by Discard Okey

If you end by discarding okey, you end by discard okey
Opponents get double score


#### Game End

If no middle piece left or a player has no piece left after discard game ends

After game ends
if player has opened gets score of the sum of their hand pieces
if player hasn't opened gets full score regardless of the hand sum
if player has a okey in hand gets full score

if player has opened pairs gets double score

if player ends the game gets negative full score
if player ends by discard okey gets negative double score


example:

end by hand x2
end by pair x2
end by discard okey x2

   o  h         mhp md = t
p1 os 10 +101   x2  x2 = 40 + 101
p2 op 20 x2     x2  x2 = 40
p3 no xx +101   x2  x2 = 404
p4 os 0 -101    x2    = -202

open type
hand sum
okey left
end type


### Okey Piece Open

open series

score

validate

examples

sign: b3
okey: b4
fake: b4

r1r2r3r4 10
r1r2b4r4 10
r4l4f0g4 16

open series r1r2r3r4 r1r2b4r4 r4l4f0g4

R1 R2 R3 R4
R1 R2 B4(R3) R4 (score)
R4 L4 F0(B4) G4 (score)

(valid R1 R2 R3 R4 10)
(valid R1 R2 B4 R4 10)
(valid L4 F0 G4    12)

table open series

drop open

R5 group 1 right

get group

(valid R1 R2 R3 R4 10)
(valid R1 R2 R3 R4 R5 15) drop R5

replace
(valid R1 R2 B4 R4 10)
(valid R1 R2 R3 R4 10) drop R3 [replace okey]
