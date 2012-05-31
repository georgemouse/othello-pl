/**
 * Copyright (c) 2008 Leo Arias elopio@softwarelibrecr.org Instituto Tecnológico de Costa Rica
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/**
 * init_board
 */
init_board(Rowi,TempBoard,Board):-
    rownum(R),
    Rowi=R,
    Board=TempBoard,
    !.

init_board(Rowi,TempBoard,Board):-
    rownum(R),
    Rowi<R, 
    init_row(Rowi,0,[],Row),
    append(TempBoard,[Row],NTempBoard),
    NRowi is Rowi+1,
    init_board(NRowi,NTempBoard,Board).

init_row(_,Coli,TempRow,Row):-
    colnum(C),
    Coli=C,
    append(TempRow,[],Row),
    !.
init_row(Rowi,Coli,TempRow,Row):-
    rownum(R),
    colnum(C),
    Coli<C,
    CenterR is C/2,
    CenterL is C/2-1,
    CenterD is R/2,
    CenterU is R/2-1,
    (
        ( (Rowi = CenterU , Coli = CenterL);
          (Rowi = CenterD , Coli = CenterR) ) ->
                Color=black
        ;
        ( (Rowi = CenterU , Coli = CenterR);
          (Rowi = CenterD , Coli = CenterL) ) ->
                    Color = white
        ;
        Color = empty
    ),
    append(TempRow,[Color],NTempRow),
    NColi is Coli+1,
    init_row(Rowi,NColi,NTempRow,Row).

/**
 * print_board
 */
print_board(Board):-
	/*cls,*/
    tab(4),
    print_head(0),
    print_body(0,0,Board),
    nl.

print_head(Coli):-
    colnum(C),
    Coli=C,
    nl,
    write('----'),
    for(1,C,1,write('--')),
    nl,
    !.

print_head(Coli):-
    write(Coli),
    tab(1),
    NColi is Coli+1,
    print_head(NColi).


print_body(Rowi,Coli,_):-
    rownum(R),
    colnum(C),
    Rowi is R-1,
    Coli is C,
    !.

print_body(Rowi,Coli,Board):-
    colnum(C),
    (
        Coli=0 ->
            write(Rowi),
            write(' | '),
            NRowi is Rowi,
            NColi is Coli+1,
            print_piece(Rowi,Coli,Board)
        ;
        Coli=C -> 
            NRowi is Rowi+1,
            NColi is 0,
            nl
        ;
            NRowi is Rowi,
            NColi is Coli+1,
            print_piece(Rowi,Coli,Board)
    ),
    print_body(NRowi,NColi,Board).

/*
 * piece
 */
print_piece(Rowi, Coli, Board):-
    piece(Board, Rowi, Coli, Piece),
    (
        Piece=black->
            write('X ')
        ;
        Piece=white ->
            write('O ')
        ;
        write('- ')
    ).

piece(Board, Rowi, Coli, Piece):-
    is_valid_index(Rowi,Coli),
    nth0(Rowi,Board,Row),
    nth0(Coli,Row,Piece).

/*
 * is_valid_index
 */
is_valid_index(Rowi,Coli):-
    rownum(R),
    colnum(C),
    Rowi>=0,
    Rowi<R,
    Coli>=0,
    Coli<C.

/*
 * is_board_full
 */
is_board_full(Board,IsBoardFull):-
    flatten(Board,PieceList),
    list_to_set(PieceList,PieceSet),
    (
        not(member(empty,PieceSet))->
            IsBoardFull = yes
        ;
        IsBoardFull = no
    ).
/*
 * empty_on_board
 */
empty_on_board(Board):-
	member(Row, Board),
	member(Piece, Row),
	Piece = empty,!.

/*
 * full_board
 */
full_board(Board):-
	flatten(Board, PiecesList),
	list_to_set(PiecesList, PiecesSet),
	not(member(empty, PiecesSet)).

/*
 * find_states
 */
find_states(State, Color, StatesList):-
	find_boards(State, Color, StatesList).

/*
 * find_boards
 */
find_boards(Board, Color, BoardsList):-
	find_moves(Board, Color, MovesList),
	find_boards(Board, Color, OrderedBoardsList, [], MovesList),
	first_elements(OrderedBoardsList, [], BoardsList).

find_boards(Board,_, BoardsList, [], []):-
	append([], [[Board, 0]], BoardsList),!.

find_boards(_, _, BoardsList, BoardsList, []):-!.

find_boards(Board, Color, BoardsList, CurrentBoardsList, [Move|RestMovesList]):-
	set_piece(Board, Move, Color, FinalBoard),
	order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList),
	find_boards(Board, Color, BoardsList, NBoardsList, RestMovesList),!.

/*
 * order_boards
 */
order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList):-
	rival_color(Color, RivalColor),
	valid_positions(FinalBoard, RivalColor, Number),
	order_boards_aux([FinalBoard, Number], CurrentBoardsList, [], NBoardsList).

order_boards_aux(Board, [], CurrentList, FinalList):-
	append(CurrentList, [Board], FinalList),!.

order_boards_aux(Board, [First|Rest], CurrentList, FinalList):-
	nth0(1, First, Value),
	nth0(1, Board, NewValue),
	NewValue =< Value,
	append(CurrentList, [Board], TempList),
	append(TempList, [First|Rest], FinalList),!.

order_boards_aux(Board, [First|Rest], CurrentList, FinalList):-
	append(CurrentList, [First], NCurrentList),
	order_boards_aux(Board, Rest, NCurrentList, FinalList),!.

/*
 * valid_positions
 */
valid_positions(Board, Color, Number):-
	valid_positions(Board, Color, 0, 0, 0, Number).

valid_positions(_, _, Rowi, Coli, Number, Number):-
    rownum(R),
    colnum(C),
    Rowi is R-1,
    Coli is C,!.

valid_positions(Board, Color, RowIndex, Coli, CurrentNumber, FinalNumber):-
    colnum(C),
    Coli is C,
	NRowIndex is RowIndex + 1,
	valid_positions(Board, Color, NRowIndex, 0, CurrentNumber, FinalNumber),!.

valid_positions(Board, Color, RowIndex, ColumnIndex, CurrentNumber, FinalNumber):-
	single_valid_move(Board, RowIndex, ColumnIndex, Color),
	NCurrentNumber is CurrentNumber + 1,
	NColumnIndex is ColumnIndex + 1,
	valid_positions(Board, Color, RowIndex, NColumnIndex, NCurrentNumber, FinalNumber),!.

valid_positions(Board, Color, RowIndex, ColumnIndex, CurrentNumber, FinalNumber):-
	NColumnIndex is ColumnIndex + 1,
	valid_positions(Board, Color, RowIndex, NColumnIndex, CurrentNumber, FinalNumber),!.

/*
 * single_valid_move
 */
single_valid_move(Board, RowIndex, ColumnIndex, Color) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	direction_offsets(DirectionOffsets),
	member(DirectionOffset, DirectionOffsets),
	nth0(0, DirectionOffset, RowOffset),
	nth0(1, DirectionOffset, ColumnOffset),
	NeighborRow is RowIndex + RowOffset,
	NeighborColumn is ColumnIndex + ColumnOffset,
	rival_color(Color, RivalColor),
	piece(Board, NeighborRow, NeighborColumn, NeighborPiece),
	NeighborPiece = RivalColor,
	find_color(Board, NeighborRow, NeighborColumn, RowOffset, ColumnOffset, Color),!.

/*
 * find_moves
 */
find_moves(Board, Color, MovesList):-
	find_moves(Board, Color, 0, 0, [], MovesList).

find_moves(_, _, Rowi, Coli, MovesList, MovesList):-
    rownum(R),
    colnum(C),
    Rowi is R-1,
    Coli is C,!.

find_moves(Board, Color, RowIndex, Coli, MovesList, FinalList):-
    colnum(C),
    Coli is C,
	NRowIndex is RowIndex + 1,
	find_moves(Board, Color, NRowIndex, 0, MovesList, FinalList),!.

find_moves(Board, Color, RowIndex, ColumnIndex, MovesList, FinalList):-
	valid_move(Board, RowIndex, ColumnIndex, Color, ValidDirectionOffsets),
	append(MovesList,[[RowIndex, ColumnIndex, ValidDirectionOffsets]], NMovesList),
	NColumnIndex is ColumnIndex + 1,
	find_moves(Board, Color, RowIndex, NColumnIndex, NMovesList, FinalList),!.

find_moves(Board, Color, RowIndex, ColumnIndex, MovesList, FinalList):-
	NColumnIndex is ColumnIndex + 1,
	find_moves(Board, Color, RowIndex, NColumnIndex, MovesList, FinalList),!.

/*
 * valid_move
 */
valid_move(Board, RowIndex, ColumnIndex, Color, ValidDirectionOffsets):-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	direction_offsets(DirectionOffsets),
	valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, [], ValidDirectionOffsets).

valid_move(_, _, _, _, [], CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	CurrentValidDirectionOffsets \= [],
	CurrentValidDirectionOffsets = ValidDirectionOffsets.

valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	DirectionOffsets = [DirectionOffset|DirectionOffsetsRest],
	valid_move_offset(Board, RowIndex, ColumnIndex, Color, DirectionOffset),
	append(CurrentValidDirectionOffsets, [DirectionOffset], NCurrentValidDirectionOffsets),
	valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsetsRest, NCurrentValidDirectionOffsets, ValidDirectionOffsets).

valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	DirectionOffsets = [_|DirectionOffsetsRest],
	valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsetsRest, CurrentValidDirectionOffsets, ValidDirectionOffsets).

valid_move_offset(Board, RowIndex, ColumnIndex, Color, DirectionOffset):-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	nth0(0, DirectionOffset, RowOffset),
	nth0(1, DirectionOffset, ColumnOffset),
	NeighborRow is RowIndex + RowOffset,
	NeighborColumn is ColumnIndex + ColumnOffset,
	rival_color(Color, RivalColor),
	piece(Board, NeighborRow, NeighborColumn, NeighborPiece),
	NeighborPiece = RivalColor,
	find_color(Board, NeighborRow, NeighborColumn, RowOffset, ColumnOffset, Color).

/*
 * find_color
 */
find_color(Board, RowIndex, ColumnIndex, RowOffset, ColumnOffset, Color) :-
	NRowOffset is RowIndex + RowOffset,
	NColumnOffset is ColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	Piece = Color.

find_color(Board, RowIndex, ColumnIndex, RowOffset, ColumnOffset, Color) :-
	NRowIndex is RowIndex + RowOffset,
	NColumnIndex is ColumnIndex + ColumnOffset,
	piece(Board, NRowIndex, NColumnIndex, Piece),
	rival_color(Color, RivalColor),
	Piece = RivalColor,
	find_color(Board, NRowIndex, NColumnIndex, RowOffset, ColumnOffset, Color).

/*
 * set_piece
 */
set_piece(Board, Move, Color, FinalBoard):-
	nth0(0, Move, Row),
	nth0(1, Move, Column),
 	nth0(2, Move, ValidDirectionOffsets),
	set_single_piece(Board, Row, Column, Color, BoardWithPiece),
	set_pieces_on_offsets(BoardWithPiece, Row, Column, Color, ValidDirectionOffsets, FinalBoard).

set_piece(Board, PieceRowIndex, PieceColumnIndex, Color, FinalBoard):-
	valid_move(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets),
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, Color, BoardWithPiece),
	set_pieces_on_offsets(BoardWithPiece, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets, FinalBoard).

set_pieces_on_offsets(FinalBoard, _, _, _, [], FinalBoard):-!.

set_pieces_on_offsets(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets, FinalBoard):-
	ValidDirectionOffsets = [ValidDirectionOffset|ValidDirectionOffsetsRest],
	set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, TempBoard),
	set_pieces_on_offsets(TempBoard, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsetsRest, FinalBoard).

set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, FinalBoard):-
	nth0(0, ValidDirectionOffset, RowOffset),
	nth0(1, ValidDirectionOffset, ColumnOffset),
	NRowOffset is PieceRowIndex + RowOffset,
	NColumnOffset is PieceColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	Piece = Color,
	Board = FinalBoard,!.

set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, FinalBoard):-
	nth0(0, ValidDirectionOffset, RowOffset),
	nth0(1, ValidDirectionOffset, ColumnOffset),
	NRowOffset is PieceRowIndex + RowOffset,
	NColumnOffset is PieceColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	rival_color(Color, RivalColor),
	Piece = RivalColor,
	set_single_piece(Board, NRowOffset, NColumnOffset, Color, TempBoard),
	set_pieces_on_offset(TempBoard, NRowOffset, NColumnOffset, Color, ValidDirectionOffset, FinalBoard).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, Color, FinalBoard):-
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, 0, 0, Color, [], FinalBoard, []).

set_single_piece(_, PieceRowIndex, _, Rowi, Coli, _, ResultingBoard, FinalBoard, PieceRow):-
    rownum(R),
    colnum(C),
    PieceRowIndex is R-1,
    Rowi is R-1,
    Coli is C,
	append(ResultingBoard, [PieceRow], FinalBoard),!.

set_single_piece(_, _, _, Rowi, 0, _, FinalBoard, FinalBoard, _):-
    rownum(R),
    Rowi is R,!.

set_single_piece(Board, PieceRowIndex, ColumnRowIndex, PieceRowIndex, Coli, Color, ResultingBoard, FinalBoard, RowIndex):-
    rownum(R),
    colnum(C),
    Coli is C,
	PieceRowIndex \= (R-1),
	NCurrentRowIndex is PieceRowIndex + 1,
	append(ResultingBoard, [RowIndex], NResultingBoard),
	set_single_piece(Board, PieceRowIndex, ColumnRowIndex, NCurrentRowIndex, 0, Color, NResultingBoard, FinalBoard, []).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, PieceColumnIndex, Color, ResultingBoard, FinalBoard, PieceRow):-
	append(PieceRow, [Color], NPieceRow),
	NCurrentColumnIndex is PieceColumnIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, NCurrentColumnIndex, Color, ResultingBoard, FinalBoard, NPieceRow).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, CurrentColumnIndex, Color, ResultingBoard, FinalBoard, PieceRow):-
	CurrentColumnIndex \= PieceColumnIndex,
	piece(Board, PieceRowIndex, CurrentColumnIndex, Piece),
	append(PieceRow, [Piece], NPieceRow),
	NCurrentColumnIndex is CurrentColumnIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, NCurrentColumnIndex, Color, ResultingBoard, FinalBoard, NPieceRow).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, CurrentRowIndex, _, Color, ResultingBoard, FinalBoard, PieceRow):-
	PieceRowIndex \= CurrentRowIndex,
	nth0(CurrentRowIndex, Board, CurrentRow),
	append(ResultingBoard, [CurrentRow], NResultingBoard),
	NCurrentRowIndex is CurrentRowIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, NCurrentRowIndex, 0, Color, NResultingBoard, FinalBoard, PieceRow).

/*
 * count_pieces
 */
count_pieces(Color,Board,Pieces,RivalPieces):-
    count_pieces(0,0,Color,Board,0,0,Pieces,RivalPieces).

count_pieces(Rowi,Coli,_,_,PiecesBuf,RivalPiecesBuf,Pieces,RivalPieces):-
    rownum(R),
    colnum(C),
    Rowi is R-1,
    Coli is C,
    Pieces = PiecesBuf,
    RivalPieces is RivalPiecesBuf,
    !.

count_pieces(Rowi,Coli,Color,Board,PiecesBuf,RivalPiecesBuf,Pieces,RivalPieces):-
    colnum(C),
    rival_color(Color,RivalColor),
    (
        Coli=C -> 
            NRowi is Rowi+1,
            NColi is 0,
            NPiecesBuf is PiecesBuf,
            NRivalPiecesBuf is RivalPiecesBuf
        ;
        NRowi is Rowi,
        NColi is Coli+1,
        piece(Board, Rowi, Coli, Piece),
        (
            Piece = Color ->
                NPiecesBuf is PiecesBuf+1,
                NRivalPiecesBuf is RivalPiecesBuf
            ;
            Piece = RivalColor ->
                NPiecesBuf is PiecesBuf,
                NRivalPiecesBuf is RivalPiecesBuf +1
            ;
            NPiecesBuf is PiecesBuf,
            NRivalPiecesBuf is RivalPiecesBuf
        )
    ),
    count_pieces(NRowi,NColi,Color,Board,NPiecesBuf,NRivalPiecesBuf,Pieces,RivalPieces).
/*
 * direction_offsets
 */
direction_offsets(OffsetsList) :-
	OffsetsList = [[-1, 0],
			[-1, 1],
			[0, 1],
			[1, 1],
			[1, 0],
			[1, -1],
			[0, -1],
			[-1,-1]].

cls :-  put(27), put("["), put("2"), put("J").

getRowCol(R,C):-
	rownum(R),
	colnum(C).
