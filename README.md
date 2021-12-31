##LIGHTS OUT Game

Environment: - Clisp compiler

The Game: - The goal of the game is to get the 3x3 board (or any other size) to be switched to all zeroes - When you switch the light, it toggles the adjacent positions - The game starts with at least one switch on

##To Run A Game To Play: 
- Open a terminal session at the location of the file.
- Run Clisp.
- Load the game file with (load "game.lisp") function.
- run (start-game-easy) If you wish to start an easy 3x3 board game.
- run (start-game-hard) If you wish to start a hard game on a 8x8 board.
- to set up the game in a different way and run it, you can use (start-game-settings-size-percentfill <board-size> <percent-fill>), where
  - <board-size> is an integer between 1 and any-integer-you-want, specifies the side-length of the board (input 3 for 3x3 board).
  - <percent-fill> is an integer between 1 and 9, specifies the percent-fill in cut notation (input 3 for 30% board fill).

The AI can play the game in the same manner the player can. the AI file overwrites the pick-a-switch function and plays until the limit of 450 moves reached.
The AI is very randomized and exhibits no special goals.
##To Run AI Game:
- repeat directions the way you would run the game to play yourself, except replace the file-name you load into clisp to (load "game-ai.lisp").
    
##Contacts: 
- @Author: Seymour Pashayev 
- @Supervisor: Erik Steinmetz
