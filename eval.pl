final(Board, Value):-
    full_board(Board),
    count_pieces(black, Board, BlackPieces, WhitePieces),
    Value is BlackPieces - WhitePieces.

/* 
 * black heuristic
 */
eval(black,Board, Value):-
    count_pieces(black, Board, BlackPieces, WhitePieces),
    HeuristicValue1 is BlackPieces - WhitePieces,
	getCorners(Corners),
	getXSquares(XSquares),
	positionCount(black,Board,Corners,BlackCorner,WhiteCorner),
	positionCount(black,Board,XSquares,BlackXSquares,WhiteXSquares),
	CornersBonus is 10*(BlackCorner-WhiteCorner),
	XSquaresBonus is -10*(BlackXSquares-WhiteXSquares),
	Bonus is CornersBonus+XSquaresBonus,
	Value is HeuristicValue1+Bonus.



/* 
 * white heuristic
 */
eval(white,Board, Value):-
    count_pieces(black, Board, BlackPieces, WhitePieces),
    HeuristicValue1 is BlackPieces - WhitePieces,
	getCorners(Corners),
	getXSquares(XSquares),
	positionCount(black,Board,Corners,BlackCorner,WhiteCorner),
	positionCount(black,Board,XSquares,BlackXSquares,WhiteXSquares),
	CornersBonus is 10*(BlackCorner-WhiteCorner),
	XSquaresBonus is -10*(BlackXSquares-WhiteXSquares),
	Bonus is CornersBonus+XSquaresBonus,
	Value is HeuristicValue1+Bonus.

final(black,Board, Value):-
    full_board(Board),
    count_pieces(black, Board, BlackPieces, WhitePieces),
    Value is BlackPieces - WhitePieces.
    
final(white,Board, Value):-
    full_board(Board),
    count_pieces(black, Board, BlackPieces, WhitePieces),
    Value is BlackPieces - WhitePieces.

/*count pieces at special position*/
positionCount(Color,Board,PositionList,Count,RivalCount):-
	positionCount(Color,Board,PositionList,0,0,Count,RivalCount).

positionCount(Color,Board,PositionList,CountBuf,RivalCountBuf,Count,RivalCount):-
	rival_color(Color,RivalColor),
	(
		PositionList=[]->
			Count=CountBuf,
			RivalCount=RivalCountBuf
		;
		PositionList = [Position|PositionsRest],
		Position = [Rowi,Coli|CheckList],
		(
			CheckList\=[]->
				CheckList=[CheckRow,CheckCol],
				piece(Board,CheckRow,CheckCol,CheckPiece)
			;
				CheckPiece=null
		),
		piece(Board,Rowi,Coli,Piece),
		(
			(Piece=Color,CheckPiece\=Color)->
				NCountBuf is CountBuf+1,
				NRivalCountBuf is RivalCountBuf
			;
			(Piece=RivalColor,CheckPiece\=RivalColor)->
				NCountBuf is CountBuf,
				NRivalCountBuf is RivalCountBuf +1
			;
				NCountBuf is CountBuf,
				NRivalCountBuf is RivalCountBuf
		),
		positionCount(Color,Board,PositionsRest,NCountBuf,NRivalCountBuf,Count,RivalCount)
	).


getCorners(Corners):-
	getRowCol(RR,CC),
	R is RR-1,
	C is CC-1,
	Corners=[[0,0],[0,C],
			 [R,0],[R,C]].
getXSquares(XSquares):-
	getRowCol(RR,CC),
	CornerR is RR-1,
	CornerC is CC-1,
	R is RR-2,
	C is CC-2,
	XSquares=[	[1,1, 0,0],
				[1,C, 0,CornerC],
			 	[R,1, CornerR,0],
				[R,C, CornerR,CornerC]
			].


