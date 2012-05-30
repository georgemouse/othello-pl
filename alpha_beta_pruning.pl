/**
 * Copyright (c) 1997-2006 Miguel Filgueiras mig@ncc.up.pt Universidade do Porto
 * Copyright (c) 2008 Leo Arias elopio@softwarelibrecr.org Instituto Tecnológico de Costa Rica
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
 * The minimax algorithm used as the base for the alpha-beta pruning was created by Miguel Filgueiras.
 * The original can be found here: http://www.ncc.up.pt/~mig/rped/search.txt
 */

/**
 * Relation: alpha_beta_pruning/5
 * Searches for a move using the alpha-beta pruning algorithm
 * @1: State - the current board
 * @2: Depth - the maximum depth of the search
 * @3: Color - the color of the player that moves next
 * @4: NewState - the board after applying the move selected by the search
 * @5: Value - the heuristic value of the move
 */
alpha_beta_pruning(State, Depth, Color, NewState, Value):- 
	alpha_beta_pruning(Depth, State, Color, NewState, Value, -1000, 1000).

/**
 * Relation: alpha_beta_pruning/7
 * Searches for a move using the alpha-beta pruning algorithm with an alpha and a beta value
 * @1: Depth - the maximum depth of the search
 * @2: State - the current state
 * @3: Color - the color of the player that moves next
 * @4: NewState - the board after applying the move selected by the search
 * @5: Value - the heuristic value of the move
 * @6: Alpha - the current best value for the player that tries to minimize the game value
 * @7: Beta - the current best value for the player that tries to maximize the game value
 */
alpha_beta_pruning(_, State, _, State, Value, _, _) :- final(State, Value),!.

/**
 * Relation: alpha_beta_pruning/7
 * Searches for a move using the alpha-beta pruning algorithm with an alpha and a beta value
 */
alpha_beta_pruning(0, State, _, State, Value, _, _) :- eval(State, Value),!.

/**
 * Relation: alpha_beta_pruning/7
 * Searches for a move using the alpha-beta pruning algorithm with an alpha and a beta value
 */
alpha_beta_pruning(Depth, State, Color, NewState, Value, Alpha, Beta) :-
	Depth > 0,
	garbage_collect,
	find_states(State, Color, StatesList),
	/*length(StatesList, L),
	writef('number of boards %d, depth %d\n', [L,Depth]),
	first_n_elements(7, BoardsList, NBoardsList),*/
	rival_color(Color, RivalColor),
	NDepth is Depth - 1,
	catch(
		alpha_beta_pruning(StatesList, NDepth, Color, RivalColor, NewState, Value, Alpha, Beta),
		_,
		alpha_beta_pruning_recover(StatesList, Depth, Color, RivalColor, NewState, Value, Alpha, Beta)).

/**
 * Relation: alpha_beta_pruning_recover/8
 * Used to recover when an exception occurs. Updates the depth to 0 to end the search
 * @1: StatesList - list of possible states
 * @2: Depth - the maximum depth of the search
 * @3: Color - the color of the player that moves next
 * @4: RivalColor - the color of the rival
 * @5: NewState - the board after applying the move selected by the search
 * @6: Value - the heuristic value of the move
 * @7: Alpha - the current best value for the player that tries to minimize the game value
 * @8: Beta - the current best value for the player that tries to maximize the game value
 */
alpha_beta_pruning_recover(StatesList, Depth, Color, RivalColor, NewState, Value, Alpha, Beta):-
	/*print_message(_, Error),*/
	writef('recovered at depth %d\n', [Depth]),
	garbage_collect,
	alpha_beta_pruning(StatesList, 0, Color, RivalColor, NewState, Value, Alpha, Beta).

/**
 * Relation: alpha_beta_pruning/8
 * Searches for a move using the alpha-beta pruning algorithm with an alpha and a beta value
 * @1: StatesList - list of possible states
 * @2: Depth - the maximum depth of the search
 * @3: Color - the color of the player that moves next
 * @4: RivalColor - the color of the rival
 * @5: NewState - the board after applying the move selected by the search
 * @6: Value - the heuristic value of the move
 * @7: Alpha - the current best value for the player that tries to minimize the game value
 * @8: Beta - the current best value for the player that tries to maximize the game value
 */
alpha_beta_pruning([State], Depth, _, RivalColor, State, Value, Alpha, Beta):- !, 
	alpha_beta_pruning(Depth, State, RivalColor, _, Value, Alpha, Beta).

/**
 * Relation: alpha_beta_pruning_recover/8
 * Searches for a move using the alpha-beta pruning algorithm with an alpha and a beta value
 */
alpha_beta_pruning([State|Rest], Depth, Color, RivalColor, NewState, Value, Alpha, Beta) :-
	alpha_beta_pruning(Depth, State, RivalColor, _, X, Alpha, Beta),
	(
		prune(Color, X, Alpha, Beta) ->
		(
			NewState = State,
			Value is X
		);
		(
			recalc(Color, X, Alpha, Beta, Nalpha, NBeta),
			alpha_beta_pruning(Rest, Depth, Color, RivalColor, B, Y, Nalpha, NBeta),
			best(Color, X, Y, State, B, NewState, Value)
		)
		
	).

/**
 * Relation: prune/4
 * Succeds if a prune has to be done
 * @1: Color - the color of the player that moves next
 * @2: Value - the value of the move
 * @3: Alpha - the current best value for the player that tries to minimize the game value	
 * @4: Beta - the current best value for the player that tries to maximize the game value
 */
prune(black, Value, _, Beta):-
	Value >= Beta.

/**
 * Relation: prune/4
 * Succeds if a prune has to be done
 */
prune(white, Value, Alpha, _):-
	Value =< Alpha.

/**
 * Relation: recalc/6
 * Recalculates alpha and beta values
 * @1: Color - the color of the player that moves next
 * @2: Value - the value of the move
 * @3: Alpha - the current best value for the player that tries to minimize the game value	
 * @4: Beta - the current best value for the player that tries to maximize the game value
 * @5: Nalpha - the new alpha
 * @6: Nbeta - the new beta
 */
recalc(black, Value, Alpha, Beta, Nalpha, Beta):-
	max_list([Alpha, Value], Nalpha).

/**
 * Relation: recalc/6
 * Recalculates alpha and beta values
 */
recalc(white, Value, Alpha, Beta, Alpha, NBeta):-
	min_list([Beta, Value], NBeta).

/**
 * Relation: best/7
 * Calculates the best value depending on the color that moves next
 */
best(black, X, Y, A, _, A, X):- X>=Y,!.
best(black, _, Y, _, B, B, Y).
best(white, X, Y, A, _, A, X):- X=<Y, !.
best(white, _, Y, _, B, B, Y).
