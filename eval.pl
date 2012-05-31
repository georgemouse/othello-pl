/*
 * final
 */
final_black(Board, Value):-
	full_board(Board),
	count_pieces(black, Board, BlackPieces, WhitePieces),
	Value is BlackPieces - WhitePieces.

final_white(Board, Value):-
    full_board(Board),
    count_pieces(black, Board, BlackPieces, WhitePieces),
    Value is BlackPieces - WhitePieces.

/*
 * eval
 */

eval_black(Board, Value):-
    count_pieces(black, Board, BlackPieces, WhitePieces),
    PieceDiff is  BlackPieces - WhitePieces,
    getCorners(Corners),
    positionCount(black,Board,Corners,CornerCount),
    HeuristicValue1 is BlackPieces - WhitePieces,
    /*valid_positions(Board, black, BlackValidMoves),
    valid_positions(Board, white, WhiteValidMoves),
    HeuristicValue2 is BlackValidMoves - WhiteValidMoves,*/
    Value = HeuristicValue1.

eval_white(Board, Value):-
    count_pieces(black, Board, BlackPieces, WhitePieces),
    HeuristicValue1 is BlackPieces - WhitePieces,
    /*valid_positions(Board, black, BlackValidMoves),
    valid_positions(Board, white, WhiteValidMoves),
    HeuristicValue2 is BlackValidMoves - WhiteValidMoves,*/
    Value = HeuristicValue1.
/*count pieces at special position*/

positionCount(Color,Board,PositionList,Count):-
	positionCount(Color,Board,PositionList,0,Count).

positionCount(Color,Board,PositionList,CountBuf,Count):-
	(
		PositionList=[]->
			Count=CountBuf
		;
		PositionList = [Position|PositionsRest],
		nth0(0,Position,Rowi),
		nth0(1,Position,Coli),
		piece(Board,Rowi,Coli,Piece),
		(
			Piece=Color->
				NCountBuf is CountBuf+1
			;
				NCountBuf is CountBuf
		),
		positionCount(Color,Board,PositionsRest,NCountBuf,Count)
	).

getCorners(Corners):-
	getRowCol(RR,CC),
	R is RR-1,
	C is CC-1,
	Corners=[[0,0],[0,C],
			 [R,0],[R,C]].
    
