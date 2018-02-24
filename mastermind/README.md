# mastermind

A small project for 3I020 (Declarative programming UE) sort of reproduce of the very famous game MasterMind as explained here :
https://en.wikipedia.org/wiki/Mastermind_(board_game)

## Installation

http://project.com/3I020 (Doesn't work though)

## Usage

Use lein if you want to launch the project.
There are two ways to launch it.

1)
In a terminal, enter the repository of the game (~/projets_3I020/mastermind).
Then use this command : lein repl.
Afterwards, this line will appear on the terminal :
mastermind.core=>
To launch the game, enter (-main) after the line above.
You should have this : mastermind.core=> (-main).
Finally enjoy the game !

2)
In a terminal, enter the repository of the file core.clj (~/projets/mastermind/src/mastermind).
Then enter this command : lein run.
Finally enjoy the game !



WARNING USE OF MIDJE
We are very sorry, but we are still trying to understand how to use midje.
If you want to run a test, please use the command test (lein test).
It is said that test and midje are linked : 
https://github.com/marick/Midje/wiki/Running-midje


## Options

Does not have any options.

## Examples

Here some examples for you :

************************if you want to guess the secret code of the computer********************** :

$~/Desktop/lein/lein run
************************************* Welcome to the MatserMind game ! Do you want to start a new game ? *************************************
 1 : yes,why not?
 2 : WTH! not at all
1
 Perfect !
 Shall I defeat you ?
 1 : Yes, try it if you dare
 2 : No, I want to defeat you !
 3 : After a thought, I don't want to play
1
 Nice, let's begin the game !
 So, let's start by entering you code, please choose 4 colors from red, blue, green, yellow, black and white
red blue green black
 How many tries do you give me ?
 1 : 12
 2 : 10
 3 : 8
 4 : 6 (Hmm, easy for me
4
 I have 6 tries left
 Is this the good answer ? If it is at the right place tap good, 
							if there is a token which color is in your code but not at the right place in mine please tap color, 
							else tap bad
 Yours :  [:red :blue :green :black]
 Mine :  [:green :green :red :red]
color bad color bad
 Oh too bad, let's retry !
 I have 5 tries left
 Is this the good answer ? If it is at the right place tap good, 
							if there is a token which color is in your code but not at the right place in mine please tap color, 
							else tap bad
 Yours :  [:red :blue :green :black]
 Mine :  [:red :black :green :blue]
good color good color
 Oh too bad, let's retry !
 I have 4 tries left
 Is this the good answer ? If it is at the right place tap good, 
							if there is a token which color is in your code but not at the right place in mine please tap color, 
							else tap bad
 Yours :  [:red :blue :green :black]
 Mine :  [:red :blue :green :black]
good good good good 
 Yes ! I did it, it was fun, thanks a lot !




************************if you want to play against your computer********************** :
$~/Desktop/lein/lein run
************************************* Welcome to the MatserMind game ! Do you want to start a new game ? *************************************
 1 : yes,why not?
 2 : WTH! not at all
1
 Perfect !
 Shall I defeat you ?
 1 : Yes, try it if you dare
 2 : No, I want to defeat you !
 3 : After a thought, I don't want to play
2
 Perfect! Will you be the next mastermind?
 How many tries do you want ?
 1 : 12 (hmmm still a newbie ?)
 2 : 10 (A good start hmm)
 3 : 8 (Oh you are very confident )
 4 : 6 (Wow , You really like challenges !)
6
Please enter an appropriate number..
4
 The Boss has chosen 4 colors from red, blue, green, yellow, black and white so let's start !
You have 6 tries left
red blue white gree,
Wrong arguments ! Please enter only one argument with the correct format : example red (do not add a space after)
red
blue
green
white
[red blue green white]
[:bad :bad :color :color]
Too bad ! let's retry
You have 5 tries left
green 
white
green 
black
[green white green black]
[:color :color :bad :color]
Too bad ! let's retry
You have 4 tries left
white 
green 
black      
yellow
[white green black yellow]
[:color :good :color :good]
Too bad ! let's retry
You have 3 tries left
black 
green 
white     
yellow
[black green white yellow]
[:good :good :good :good]
 OMG ! Congratulations! I can't believe it ! You won ! Another game ? 
 1 : it was fun, so yes
 2 : I need a break, so no thanks
2
Too bad, it was fun ! See you later !



### Bugs

We have not found any bugs yet ! 
But if you have spotted any problems, please signal us :) !

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2018 3I020_MasterMind

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
