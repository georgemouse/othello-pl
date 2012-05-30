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
 * Relation: play/1
 * Starts the game
 * @1: Depth - the maximum depth of the search
 */
play(Depth):-
    play(Depth,8,8).

play(Depth,Rown,Coln):-
    /*init global variable*/
    (
        /* if row and col is not even, print "not valid"*/
        (0 is Rown mod 2, 0 is Coln mod 2) ->
		    asserta(rownum(Rown)),
		    asserta(colnum(Coln)),
		    init_board(0,[],Board),
		    select_mode(Mode),
		    (
			    Mode is 5 ->
	                game_loop(Board,4,Depth,white)
	            ;
                    game_loop(Board,Mode,Depth,black)
            ),
		    /*clean global variable*/
		    retract(rownum(Rown)),
		    retract(colnum(Coln))
	    ;
			writeln('Not a valid row/col number, should be both even.'),
			writeln('')
    ).

/**
 * Relation: game_loop/4
 * Loops the game, switching turns, until the game ends
 * @1: Board - the current board
 * @2: Mode - the game mode.
 *	1 for human vrs machine
 *	2 for machine vrs human
 *	3 for human vrs human
 * @3: Depth - the maximum depth of the search
 * @4: Color - the color of the player that moves next
 */
game_loop(Board, Mode, Depth, Color):-
    print_board(Board),
    print_player(Color),
    is_board_full(Board,IsBoardFull),
    (
        IsBoardFull = yes ->
            show_statistics(Board)
        ;
        Mode = 1 ->
        (
            Color = black ->
                find_moves(Board, black, MovesList),
			    member(_, MovesList),
			    human_select_move(Move, MovesList),!,
			    set_piece(Board, Move, black, FinalBoard),
			    game_loop(FinalBoard, 1, Depth, white),!
            ;
            Color = white ->
                find_moves(Board, white, MovesList),
                member(_, MovesList),
			    machine_select_move(Board, Depth, white, FinalBoard),!,
			    game_loop(FinalBoard, 1, Depth, black),!
        )
        ;
        Mode = 2 ->
        (
            Color = black ->
                find_moves(Board, black, MovesList),
                member(_, MovesList),
			    machine_select_move(Board, Depth, black, FinalBoard),!,
			    game_loop(FinalBoard, 2, Depth, white),!
            ;
            Color = white ->
			    find_moves(Board, white, MovesList),
			    member(_, MovesList),
			    human_select_move(Move, MovesList),!,
			    set_piece(Board, Move, white, FinalBoard),
			    game_loop(FinalBoard, 2, Depth, black),!
        )
        ;
        Mode = 3 ->
		    find_moves(Board, Color, MovesList),
		    member(_, MovesList),
		    human_select_move(Move, MovesList),!,
		    set_piece(Board, Move, Color, FinalBoard),
		    rival_color(Color, RivalColor),
		    game_loop(FinalBoard, 3, Depth, RivalColor),!
		;
        Mode = 4 ->
        (
            Color = black ->
                find_moves(Board, black, MovesList),
                member(_, MovesList),
                machine_select_move(Board, Depth, black, FinalBoard),!,
                game_loop(FinalBoard, 4, Depth, white),!
            ;
            Color = white ->
                find_moves(Board, white, MovesList),
                member(_, MovesList),
                machine_select_move(Board, Depth, white, FinalBoard),!,
                game_loop(FinalBoard, 4, Depth, black),!
        )
    ).

show_statistics(Board):-
    nl,
    count_pieces(black, Board, NumBlack, NumWhite),
    writef('black: %d\n',[NumBlack]),
    writef('white: %d\n',[NumWhite]),
    abs(NumBlack-NumWhite, Diff),
    (
        NumBlack > NumWhite ->
            writef('black win %d piece\n', [Diff])
        ;
        NumBlack < NumWhite ->
            writef('white win %d piece\n', [Diff])
        ;
        write('Tie game\n')

    ),
    nl.

game_loop(Board, Mode, Depth, Color):-
	find_moves(Board, Color, MovesList),!,
	not(member(_,MovesList)),!,
    rival_color(Color, RivalColor),
	(
        /* if rival also have no move, show statistics*/
        (find_moves(Board, RivalColor, RivalMovesList), member(_,RivalMovesList))->	
		    writeln('There\'s no valid move.'),
			game_loop(Board, Mode, Depth, RivalColor),!
		;
            writeln('There\'s no valid move for both players.'),
            show_statistics(Board)
	).

/**
 * Relation: print_player/1
 * Prints the player that moves next
 * @1: Color - the color of the player that moves next
 */
print_player(white):-
    nl,
	writeln('White player''s turn (0)'),!.

print_player(black):-
    nl,
	writeln('Black player''s turn (X)'),!.

/**
 * Relation: human_select_move/2
 * Succeds when Move unifies with one of the possible moves in MovesList
 * @1: Move - The move selected by the human
 * @2: MovesList - The list of possible moves
 */
human_select_move(Move, MovesList):-
	write('Enter the Row: '),
	read(SelectedRow),
	writeln('Enter the Column: '),
	read(SelectedColum),
	member(Move, MovesList),
	nth0(0, Move, SelectedRow),
	nth0(1, Move, SelectedColum).

human_select_move(Move, MovesList):-
	writeln('Not a valid move'),
	writeln(''),
	human_select_move(Move, MovesList).

/**
 * Relation: machine_select_move/4
 * Selects a move from the possible ones for the player that has the next move
 * @1: Board - The current board
 * @2: Depth - The maximum depth of the search
 * @3: Color - The color of the player that moves next
 * @4: FinalBoard - The board after applying the move selected by the machine
 */
machine_select_move(Board, Depth, Color, FinalBoard):-
	garbage_collect,
	alpha_beta_pruning(Board, Depth, Color, FinalBoard, _).

/**
 * select_mode & enter_mode
 */
select_mode(Mode):-
	writeln('Select a game mode'),
	writeln('1. human vs machine'),
	writeln('2. machine vs human'),
	writeln('3. human vs human'),
    writeln('4. machine vs machine (black first)'),
    writeln('5. machine vs machine (white first)'),
	write('Enter a number: '),
	read(SelectedMode),
	(
        SelectedMode is 1 ->
		    Mode is SelectedMode,
		    writeln('machine vs human selected'),
		    writeln(''),!
        ;
        SelectedMode is 2 ->
		    SelectedMode is 2,
		    Mode is SelectedMode,
		    writeln('human vs machine selected'),
		    writeln(''),!
        ;
        SelectedMode is 3 ->
            Mode is SelectedMode,
            writeln('human vs human selected'),
            writeln(''),!
        ;
        SelectedMode is 4 ->
            Mode is SelectedMode,
            writeln('machine vs machine (black first) selected'),
            writeln(''),!
        ;
        SelectedMode is 5 ->
            Mode is SelectedMode,
            writeln('machine vs machine (white first) selected'),
            writeln(''),!
        ;
		    writeln('Not a valid mode'),
		    writeln(''),
		    select_mode(Mode)
	).

/**
 * rival_color
 */
rival_color(white, black).
rival_color(black, white).

