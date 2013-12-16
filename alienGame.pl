%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% game.pl  
%
% Use this as the starting point for your game.  This starter code includes
% the following:
%  - Two example areas
%  - An example of how you might connect those areas 
%  - Handling of the actions 'go _______.', 'help.', and 'quit.'
%  - Basic input processing which strips punctuation and puts the words into a list 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(readln)).

% Use this dynamic fact to store the player's current location
:- dynamic current_room/1.	
:- dynamic location_in_room/1.
:- dynamic object/8.
:- dynamic playerInv/1.
:- dynamic puzzles/1.

playerInv([]).
puzzles([]).

valid(north).
valid(south).
valid(east).
valid(west).
success(yes).

%%%%%%%%%%%% OPERATING ROOM %%%%%%%%%%%%%%
object(operatingRoom, north, alien, alive, heavy, [butcherknife, whiterobe, ' '], 'An alien is lunging forward, trying to grab you', scalpel).
object(operatingRoom, south, medicalOffice, north, door, locked, 'You are next to a sliding metal door', [' ']).
object(operatingRoom, east, table, open, heavy, [scalpel, ' '],'There is a small table with a scalpel on it.', nothing).
object(operatingRoom, west, shelves, open, heavy, [keycard, alieneye, ' '], 'A wall lined with shelves',nothing).

%%%%%%%%%%%%%% MEDICAL OFFICE  %%%%%%%%%%%%%
object(medicalOffice, north, operatingRoom, south, door, locked, 'You are next to a sliding metal door', [' ']).
object(medicalOffice, south, desk, unlocked, heavy, [laserpointer, ' '], 'A small desk sits against the wall', scalpel).
object(medicalOffice, west, alien, alive, heavy, [alienhead, alienarm, ' '], 'An alien is standing with his back to you, dissecting your severed left hand!!', butcherKnife).
object(medicalOffice, east, laboratory, west, door, locked, 'You are next to a sliding metal door', [' ']).

%%%%%%%%%%%%% LABORATORY %%%%%%%%%%%%%%%
object(laboratory, west, medicalOffice, east, door, locked, 'You are next to a sliding metal door', [' ']).
object(laboratory, east, cafeteria, west, door, unlocked, 'You are next to a sliding metal door', [' ']).
object(laboratory, north, headMachine, unlocked, heavy,[' '], 'A strange machine that you can place a head in and lay down inside...', alienhead).
object(laboratory, south, armMachine, unlocked, heavy, [' '], 'A small machine that you can place an arm in...', arm).

%%%%%%%%%%%%%%% CAFETERIA %%%%%%%%%%%%%%
object(cafeteria, south, hallway, north, door, unlocked, 'You are next to a revolving door', [' ']).
object(cafeteria, north, aliens, alive, heavy, [' '], 'Aliens at a table eating goo..', nothing).
object(cafeteria, west, laboratory, east, door, unlocked, 'You are next to a sliding metal door', [' ']).
object(cafeteria, east, table, open, heavy, [bluegoo, ' '], 'A table with bluegoo on platters', nothing).

%%%%%%%%%%%%%%% HALLWAY %%%%%%%%%%%%%%%%
object(hallway, north, cafeteria , south, door, unlocked, 'You are next to a revolving door', [' ']).
object(hallway, south, controlDeck, north, door, locked, 'You are next to a sliding metal door', [' ']).
object(hallway, west, sleepingRoom, east, door, unlocked, 'You are next to a sliding metal door', [' ']).
object(hallway, east, armory, west, door, locked, 'You are next to a door that has a scanner on it', [' ']).

%%%%%%%%%%% SLEEPING QUARTERS %%%%%%%%%%%
object(sleepingRoom, west, storageCloset, east, door, unlocked, 'You are next to a glass door', [' ']).
object(sleepingRoom, east, hallway, west, door, unlocked, 'You are next to a sliding metal door', [' ']).
object(sleepingRoom, north, cabinets, unlocked, heavy,[uniform,' '], 'A wall of cabinets...', nothing).
object(sleepingRoom, south, beds, unlocked, heavy, [' '], 'Aliens hanging from the ceiling...appear to be sleeping', nothing).

%%%%%%%%%%% STORAGE CLOSET %%%%%%%%%%%
object(storageCloset, east, sleepingRoom, west, door, unlocked, 'You are next to a sliding metal door', [' ']).
object(storageCloset, west, bins, unlocked, light, [boots,' '], 'multiple bins stacked up...', nothing).
object(storageCloset, north, paper, unlocked, heavy,[paper,' '], 'A bunch of very flammable paper products hmmm...', nothing).
object(storageCloset, south, dirtyclothes, unlocked, heavy, [largekey, ' '], 'A bin full of dirtyclothes', nothing).

%%%%%%%%%%% ARMORY %%%%%%%%%%%
object(armory, west, hallway, east, door, locked, 'You are next to a sliding metal door', [' ']).
object(armory, east, helmets, unlocked, light, [helmet,shields, ' '], 'Protective head gear and shields', nothing).
object(armory, north, weapons, unlocked, heavy,[bazooka, lightsaber,' '], 'Weapons on the wall...', nothing).
object(armory, south, trophies, unlocked, heavy, [trophy,' '], 'Trophies and medals in a case...', nothing).

%%%%%%%%%%%%%%% CONTROL DECK %%%%%%%%%%%%%%%%%%%
object(controlDeck, north, hallway, south, door, locked, 'You are next to a sliding metal door', [' ']).
object(controlDeck, west, teleporterRoom, east, door, unlocked, 'You are at the teleportation room door', [' ']).
object(controlDeck, east, controls, locked, heavy, [' '], 'A board with keys written in weird characters..', nothing).
object(controlDeck, south, cabinet, unlocked, heavy, [secretcode,' '], 'The alien leader''s control chair and cabinet...', nothing).

%%%%%%%%%%%%%%% TELEPORTER ROOM %%%%%%%%%%%%%%%%%%%
object(teleporterRoom, west, teleporter, east, door, unlocked, 'You are at the teleporter door', [' ']).
object(teleporterRoom, east, controlDeck, west, door, unlocked, 'You are next to the control deck door', [' ']).
object(teleporterRoom, north, alien, alive, heavy, [crystal,' '], 'A massive alien, with 4 arms is coming right for you!!', nothing).
object(teleporterRoom, south, alien, alive, heavy, [meth,' '], 'An alien is raising his gun about to shoot!!', nothing).

%%%%%%%%%%%%%%% DOORS %%%%%%%%%%%%%%%%%%%
door(operatingRoom, south, medicalOffice, north, [whiterobe], 'You can''t run around naked!!', keycard).
door(medicalOffice, north, operatingRoom, south, [], 'Can''t go in there', keycard).
door(medicalOffice, east, laboratory, west, [killofficealien], 'You need to kill the alien before he sees you''re missing..',keycard ).
door(laboratory, west, medicalOffice, east, [], 'Can''t go in there',keycard).
door(laboratory, east, cafeteria, west, [onalienhead, onalienarm], 'You need to disguise yourself better..', keycard).
door(cafeteria, west, laboratory, east, [], 'Can''t go in there',keycard).
door(cafeteria, south, hallway, north, [eatgoo], 'You''re too exhausted you need food..', keycard).
door(hallway, north, cafeteria, south, [], 'Can''t go in there', keycard).
door(hallway, west, sleepingRoom, east, [], 'Can''t go in there', keycard).
door(hallway, east, armory, west, [], 'Can''t go in there', alieneye).
door(hallway, south, controlDeck, north, [uniform, boots, helmet], 'You don''t have the right attire to be allowed in...', largekey).
door(sleepingRoom, east, hallway, west, [], 'Can''t go in there', keycard).
door(sleepingRoom, west, storageCloset, east, [], 'Can''t go in there', keycard).
door(storageCloset, east, sleepingRoom, west, [], 'Can''t go in there', keycard).
door(armory, west, hallway, east, [], 'Can''t go in there', alieneye).
door(controlDeck, north, hallway, south, [], 'Can''t go in there', largekey).
door(controlDeck, west, teleporterRoom, east, [], 'Can''t go in there', largekey).
door(teleporterRoom, east, controlDeck, west, [], 'Can''t go in there', largekey).
door(teleporterRoom, west, teleporter, east, [activated], 'You need to activate the teleporter first!!', largekey).

%%%%%%%%%%%%%% ROOMS %%%%%%%%%%%%%%%%%%%%
room(operatingRoom,'You are in a room used for dissection'). 
room(medicalOffice,'This room is very bright... it makes your eyes sting.').
room(laboratory, 'Looks like some type of lab...').
room(cafeteria, 'There are aliens eating bluegoo...').
room(hallway, 'You stand in a large hallway...').
room(sleepingRoom, 'The room is very dim, you have to squint to see where you are going...').
room(storageCloset, 'It is a little messy in here..').
room(armory, 'Now this is the kinda room you like to see!!').
room(controlDeck, 'Everything looks official in here..').
room(teleporterRoom, 'This might by your way outa here!!').
room(teleporter, 'Congratulations, you escaped the alien ship!!').
			

item(keycard, Room, Loc):-		
	door(Room, Loc, X, Y, List, D, keycard),
	door(A, B, Room, Loc, List2, E, keycard),
	object(Room, Loc, X, Y, door, locked, Description, Item),
	object(A, B, Room, Loc, door, locked, Desc, I),
	retract(object(Room, Loc, X, Y, door, locked, Description, Item)),
	assertz(object(Room, Loc, X, Y, door, unlocked, Description, Item)),
	retract(object(A, B, Room, Loc, door, locked, Desc, I)),
	assertz(object(A, B, Room, Loc, door, unlocked, Desc, I)), nl, tab(2),
	write('Door is unlocked'), nl.
	
item(largekey, Room, Loc):-		
	door(Room, Loc, X, Y, List, D, largekey),
	door(A, B, Room, Loc, List2, E, largekey),
	object(Room, Loc, X, Y, door, locked, Description, Item),
	object(A, B, Room, Loc, door, locked, Desc, I),
	retract(object(Room, Loc, X, Y, door, locked, Description, Item)),
	assertz(object(Room, Loc, X, Y, door, unlocked, Description, Item)),
	retract(object(A, B, Room, Loc, door, locked, Desc, I)),
	assertz(object(A, B, Room, Loc, door, unlocked, Desc, I)), nl, tab(2),
	write('Door is unlocked'), nl.

item(alieneye, Room, Loc):-		
	door(Room, Loc, X, Y, List, D, alieneye),
	door(A, B, Room, Loc, List2, E, alieneye),
	object(Room, Loc, X, Y, door, locked, Description, Item),
	object(A, B, Room, Loc, door, locked, Desc, I),
	retract(object(Room, Loc, X, Y, door, locked, Description, Item)),
	assertz(object(Room, Loc, X, Y, door, unlocked, Description, Item)),
	retract(object(A, B, Room, Loc, door, locked, Desc, I)),
	assertz(object(A, B, Room, Loc, door, unlocked, Desc, I)), nl, tab(2),
	write('Door is unlocked'), nl.
	
item(scalpel, Room, Loc):- 
	object(Room, Loc, alien, A, B, C, D, E),
	retract(object(Room, Loc, alien, alive, B, C, D, E)),
	assertz(object(Room, Loc, alien, dead, B, C, 'A dead Alien is on the ground.. a scalpel protruding from it''s eye.', E)),
	remove_inv(scalpel), nl, tab(2),
	write('You stabbed the alien in the eye with the scalpel!! It drops to the floor dead...'), nl.

item(butcherknife, Room, Loc):- 
	object(Room, Loc, alien, A, B, C, D, E),
	retract(object(Room, Loc, alien, alive, B, C, D, E)),
	assertz(object(Room, Loc, alien, dead, B, C, 'A decapitated Alien lies in a pool of bright green blood...', E)),
	add_puzzles(killofficealien),
	remove_inv(butcherknife),nl, tab(2),
	write('You chopped off the aliens head!!'), nl.
	
item(bazooka, Room, Loc):- 
	object(Room, Loc, alien, A, B, C, D, E),
	retract(object(Room, Loc, alien, alive, B, C, D, E)),
	assertz(object(Room, Loc, alien, dead, B, C, 'Green blood and guts are splattered everywhere...', E)),
	add_puzzles(killalien2),
	remove_inv(bazooka),nl, tab(2),
	write('Booooom, Alien guts are everywhere now!!'), nl.
	
item(lightsaber, Room, Loc):- 
	object(Room, Loc, alien, A, B, C, D, E),
	retract(object(Room, Loc, alien, alive, B, C, D, E)),
	assertz(object(Room, Loc, alien, dead, B, C, 'An Alien lies split in half, you''ve never seen so much green blood...', E)),
	add_puzzles(killofficealien),
	remove_inv(lightsaber),nl, tab(2),
	write('You slice the alien in half, the sword hisses as it burns alien flesh!!'), nl.
	
item(alienhead, Room, Loc):- 
	object(Room, Loc, headMachine, A, B, C, D, E),
	add_puzzles(onalienhead),
	remove_inv(alienhead),nl, tab(2),
	write('You lie down in the machine with the alien head on your face, it fuses to your body!!'), nl.
	
item(secretcode, Room, Loc):- 
	playerInv(X),
	isMember(secretcode,X),
	object(controlDeck, east, controls, A, B, C, D, E),
	puzzles(Inv),
	isMember(startfire, Inv),
	add_puzzles(activated),nl, tab(2),
	write('You punch in the code''s strange characters... you here a buzzing in the next room...'), nl.
	
item(board, Room, Loc):- 
	playInv(Inv),
	isMember(secretcode, Inv),
	object(controlDeck, east, controls, A, B, C, D, E),
	puzzles(Inv),
	isMember(startfire, Inv),
	add_puzzles(activated),nl, tab(2),
	write('You punch in the code''s strange characters... you hear a buzzing in the next room...'), nl.
	
item(board, Room, Loc):- 
	object(controlDeck, east, controls, A, B, C, D, E),nl, tab(2),
	write('There are too many aliens in here... you need a distraction...'), nl.


item(secretcode, Room, Loc):-
	write('Too many people in the room... you need a distraction...'), nl.
	
item(laserpointer, Room, Loc):- 
	playerInv(X),
	isMember(laserpointer,X),
	object(Room, Loc, paper, A, B, C, D, E),
	add_puzzles(startfire),
	remove_inv(laserpointer),nl, tab(2),
	write('You started a fire, get outa here fast!!!...'), nl.
	
item(alienarm, Room, Loc):- 
	object(Room, Loc, armMachine, A, B, C, D, E),
	add_puzzles(onalienarm),
	remove_inv(alienarm),nl, tab(2),
	write('You place the alien arm in with your shoulder, it fuses to your body!!'), nl.
		
item(helmet,Room, Loc):- 
		playerInv(X),
		isMember(helmet,X),
		remove_inv(helmet),
		add_puzzles(helmet),nl, tab(2),
		write('You are now wearing a helmet...'),nl.
		
item(uniform,Room, Loc):- 
		playerInv(X),
		isMember(uniform,X),
		remove_inv(uniform),
		add_puzzles(uniform),nl, tab(2),
		write('This uniform makes you look like an alien officer...'),nl.
		
item(boots,Room, Loc):- 
		playerInv(X),
		isMember(boots,X),
		remove_inv(boots),
		add_puzzles(boots),nl, tab(2),
		write('These boots sure are comfy...'),nl.
		
item(whiterobe,Room, Loc):- 
		playerInv(X),
		isMember(whiterobe,X),
		remove_inv(whiterobe),
		add_puzzles(whiterobe),nl, tab(2),
		write('You look like a bloodied surgeon...'),nl.

item(bluegoo):- 
		playerInv(X),
		isMember(bluegoo,X),
		remove_inv(bluegoo),
		add_puzzles(eatgoo),nl, tab(2),
		write('Yuckkk that tastes groose, but you feel stronger...'),nl.
		
item(bluegoo):-
		write('You don''t have blue goo in your inventory..'), nl.

item(X,Y,Z):-
	write('Doesn''t work...you''re missing something'), nl.



checkPuzzles([],Description).
checkPuzzles([H|T], Description):- 
						puzzles(Inv),
						isMember(H, Inv),
						checkPuzzles(T, Description).
						
checkPuzzles(List,Description):- tab(2), write(Description), nl, fail.
	

open(door):-		current_room(teleporterRoom),
					location_in_room(west),
					door(teleporterRoom, west, teleporter, east, List, D, E),
					object(teleporterRoom, west, teleporter, east, door, unlocked, Description, Tool),
					checkPuzzles(List, D),nl, tab(2),
					write('Congrats you escaped the alien ship!!!'), nl, tab(2),
					write('You win the Game!!!!'), nl, nl, nl,
					quit.
	
open(door):-		current_room(Room),
					location_in_room(Loc),
					door(Room, Loc, NextRoom, NextLoc, List, D, E),
					object(Room, Loc, NextRoom, NextLoc, door, unlocked, Description, Tool),
					checkPuzzles(List, D),
					retract(current_room(Room)),
					assertz(current_room(NextRoom)),
					retract(location_in_room(Loc)),
					assertz(location_in_room(NextLoc)),
					room(NextRoom,Desc),nl, tab(2),
					write(Desc), nl.
									

open(door):-		
	current_room(Room),
	location_in_room(Loc),
	object(Room,Loc, A, B, door, locked, Description, Tool),nl, tab(2),
	write('Can''t open the door, it''s locked'),nl.	
							

open(desk):-	current_room(Room),
					location_in_room(Loc),
					object(Room, Loc, desk, unlocked, _, List, Description,_),
					printList(List).
				
open(desk):- 	current_room(Room),
					location_in_room(Loc),
					object(Room, Loc, desk, locked, _, List, Description, Tool),nl, tab(2),	
					write('The desk is locked'),nl, tab(2),
					write('You need a '),nl, tab(2),
					write(Tool),
					fail.
					
use(board):- write('You need use some kinda code to get it to work'), nl.
				
use(X):-		playerInv(Y),
				isMember(X,Y),
				current_room(Room),
				location_in_room(Loc),
				item(X, Room, Loc).
											
use(X):-		write('You dont have that or it won''t work'), nl,
				fail.

eat(X):-		playerInv(Y),
				isMember(X,Y),
				item(X).

				
take(scalpel):-	current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, A, B, C, List, D, E),
			isMember(scalpel, List),
			remove(scalpel, List, R),
			retract(object(Room, Loc, A, B, C, List, D, E)),
			assertz(object(Room, Loc, A, B, C, R, 'There is a small table', E)),
			add_inv(scalpel),nl, tab(2),
			write('Added '),nl, tab(2),
			write('scalpel'), nl.
			
			
take(X):-	current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, A, B, door, D, E, List),
			isMember(X, List),
			remove(X, List, R),
			retract(object(Room, Loc, A, B, door, D, E, List)),
			assertz(object(Room, Loc, A, B, door, D, E, R)),
			add_inv(X),nl, tab(2),
			write('Added '),nl, tab(2),
			write(X), nl.
			
take(X):-	current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, A, B, C, List, D, E),
			isMember(X, List),
			remove(X, List, R),
			retract(object(Room, Loc, A, B, C, List, D, E)),
			assertz(object(Room, Loc, A, B, C, R, D, E)),
			add_inv(X),nl, tab(2),
			write('Added '),nl, tab(2),
			write(X), nl.


			
take(X):-
			write('Take What? Not possible.'), nl.

search:-	current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, X, alive, A, D, E, F),nl, tab(2),
			write('Now way jose, the alien will catch you... '), nl.
			
search:-	current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, X, B, door, D, E, F),
			printList(F), nl.
			
search:- 	current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, A, B, C, D, E, F),
			printList(D), nl.	
			
search:-	nl, tab(2), write('There is nothing here'),nl.

search(alien):-	current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, alien, alive, A, D, E, F), nl, tab(2),
			write('No way jose, the alien will catch you... '), nl.
			
search(X):-	current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, X, B, C, D, E, F),
			printList(D), nl.	
			
search(door):-
			current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, X, B, door, D, E, F),
			printList(F), nl.			
			
search(X):-	nl, tab(2), write('There is nothing here'),nl.

drop(Item):- current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, X, B, door, D, E, F),
			append(F, [Item], Y),
			retract(object(Room, Loc, X, B, C, D, E, F)),
			assertz(object(Room, Loc, X, B, C, D, E, Y)),
			remove_inv(Item), tab(2), nl,
			write('dropped '),
			write(Item), nl.
						
drop(Item):- current_room(Room),
			location_in_room(Loc),
			object(Room, Loc, X, B, C, D, E, F),
			append(D, [Item], Y),
			retract(object(Room, Loc, X, B, C, D, E, F)),
			assertz(object(Room, Loc, X, B, C, Y, E, F)),
			remove_inv(Item), tab(2), nl,
			write('dropped '),
			write(Item), nl.
			
quit:- 	abort.
			
% This rule starts everything off
play :-
	write('You wake up on an operating table'), nl, tab(2),
	write('You find yourself naked with strange markings all over your body...'), nl, tab(4),
	write('Your left arm has been removed!!!'), nl, nl,
   retractall(current_room(_)),
   retractall(location_in_room(_)),
   assertz(current_room(operatingRoom)),
   assertz(location_in_room(east)),
   print_location,
   dispPrompt,
   get_input.

% Prints out the players current location description
print_location :-	current_room(Room),
					location_in_room(Loc),
					object(Room, Loc,_,_,_,_, Description,_), nl, tab(2), write(Description), nl. 

% Changes the players current location, validity of change is checked earlier
change_area(NewArea) :-
    location_in_room(Current),
    retract(location_in_room(Current)),
    assertz(location_in_room(NewArea)).

% Displays the player prompt so they can enter actions
dispPrompt :- prompt(_, '> ').

% Handling of the action 'go _______', and a good example of how you might implement others
process_input([go, Direction]) :-
    current_room(Room),
    location_in_room(Loc),
	valid(Direction),
    change_area(Direction),
	print_location.

process_input([go, _]) :-
    print('You hit an invisible wall and can\'t go that way'), nl, nl.
	
process_input([open, Thing]):-
	open(Thing).

process_input([use, Item]):-
	use(Item).

process_input([take, Item]):-
	take(Item).
	
process_input([drop, Item]):-
	drop(Item).
	
process_input([inventory]):-
	inventory.

process_input([search, Item]):-
	search(Item).
	
process_input([search]):-
	search.
	
process_input([eat, Item]):-
	eat(Item).
	
process_input([location]):-
	print_location.

% Add some help output here to explain how to play your game
process_input([help]) :-	print('Actions:'),nl, tab(2),
							print('go ie. go north'),nl, tab(2),
							print('eat ie. eat poop.'), nl, tab(2), 
							print('search.'), nl, tab(2), 
							print('inventory.'), nl, tab(2), 
							print('open ie open door.'), nl, tab(2), 
							print('use ie use lightsaber.'), nl, tab(2), 
							print('drop ie drop girlfriend.'), nl.
		
process_input([_]) :-
    print('...try something else'), nl, nl.



%%%% Below is just some basic input handling, you probably don't have to mess with it %%%%

% Get input from the user
get_input :- read_sentence(Input), get_input(Input).
get_input([quit]).
get_input(Input) :-
    process_input(Input), %print_location,
    read_sentence(Input1), get_input(Input1).
	
% Reads a sentence from the prompt
read_sentence(Input) :-
    readln(Input1, _, ".!?", "_0123456789", lowercase),
    strip_punctuation(Input1, Input).

% Strips punctuation out of the user input
strip_punctuation([], []).
strip_punctuation([Word|Tail], [Word|Result]) :-
    \+(member(Word, ['.', ',', '?', '!'])),
    strip_punctuation(Tail, Result).
strip_punctuation([_|Tail], Result) :-
    strip_punctuation(Tail, Result).
    
    
inventory:- playerInv(X),
			printList(X).

printList([]).
printList([H|T]):- tab(4), write(H), nl, printList(T).
    
isMember(X,[X|_]).
isMember(X,[_|R]) :- isMember(X,R).

append([],X,X).
append([H|T1],X,[H|T2]) :-	append(T1,X,T2).

remove(X,[X|R],R).
remove(X,[F|R],[F|S]) :- remove(X,R,S).

add_inv(Item):- 	playerInv(X),
					append(X, [Item], Y),
					retract(playerInv(X)),
					assertz(playerInv(Y)).
						
add_puzzles(Item):- 	puzzles(X),
						append(X, [Item], Y),
						retract(puzzles(X)),
						assertz(puzzles(Y)).
						
remove_inv(Item):-		playerInv(X),
						remove(Item, X, R),
						retract(playerInv(X)),
						assertz(playerInv(R)).
							
