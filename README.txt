To start the game you must open the swi-prolog program.

In GNU / Linux, open terminal, navigate to the program folder and run swipl.
Then refer to the program with [Othello]., Then call init. to load the other files.
In Windows, run the program using the start menu and see all files.

Start the game
================
The game begins with the relationship play / 1, the value you receive is the maximum depth of the search.
The maximum depth that has been successfully tested is 5, any number greater than this could
make the search runs out of memory at some point in the game.

? play (5).

Select the mode
===================
You must select one of the modes. The mode is selected by entering the corresponding number
point and then enter.

Select a game mode
1. human vs. machine
2. human vs. machine
3. human vs. human
Enter a number:
1.

Select the play
=====================
You must choose the number of row and column in which you want to place a tab. As in mode selection is
escribiendod choose the number, then point and enter.

Black player turn (X)
Enter the Row: 4.
Enter the Column: 5.