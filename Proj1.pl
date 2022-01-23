              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                             
              %                                                 %
              %              COMP30020 - Project 1              %
              %                2021 - Semester 2                %
              %                                                 %
              %             Paul Hutchins - 1160468             %
              %                                                 %
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This code takes an empty Fill-In puzzle and a wordlist, and returns true or
% false dependant on wether or not the puzzle is solvable with the given
% wordlist


% The SWI Prolog library provides two different, incompatible transpose/2
% predicates, and loads the wrong one by default. This makes sure the correct 
% one is loaded
:- ensure_loaded(library(clpfd)).


% puzzle_solution(Puzzle, WordList).
% Entry point for the program. Puzzle contains empty puzzle, WordList
% contains WordList to solve puzzle with. Solution will be written into
% puzzle, if the puzzle is not solvable the predicate will fail.
puzzle_solution(Puzzle, WordList) :-
    fill_puzzle(Puzzle, FillArray),
    flatten_to_words(FillArray, WordSlots),
    solve_puzzle(WordSlots, WordList).


% fill_puzzle(+Puzzle, -FillArray)
% Creates Filled puzzle, which mimics the real Puzzle but has logical variables
% inplace of '_'
fill_puzzle(Puzzle, Filled) :-
    Filled = Puzzle.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Flatten_To_words                               %
%    This section of code takes the puzzle and makes a list of all wordslots   %

% flatten_to_words(+FillArray, -Flatten)
% Takes puzzle filled with logic variables, and flattens to an array of slots
% That can fit possible words.
% Only works on rows, so takes puzzle, finds slots, transposes, and finds slots
% again, then finally combines lists and stops after first solution found, as we 
% dont want permutations of solutions
flatten_to_words(FillArray, Flatten) :-
    to_slots(FillArray, HFlatten),
    transpose(FillArray, TFillArray),
    to_slots(TFillArray, VFlatten),
    combine_lists(HFlatten, VFlatten, Flatten),
    !.


% to_slots(+FillArray, -Flattened)
% Takes FillArray, passes first row in FillArray to row_to_slots to obtain slots
% in the row, appends to Flattenen and recalls to_slots

% Base case to return when every row in FillArray has been checked
to_slots([], _).

% Recursive case which solves row, adds row to Flattened array, and calls
% to_slots again on the rest of the FillArray
to_slots([H|FillArray], Flattened) :-
    row_to_slots(H, [], Slots),
    append(Slots, Temp, Flattened),
    to_slots(FillArray, Temp).

% row_to_slots(+Row, +CurrentSlot, -SlotList)
% Takes a row and iterates through the row to create a list of all Slots on the
% row, a slot starts at the start of a row, or after a '#' and a slot ends at
% a '#' or the end of a row.

% First base case, if slot is > length 1 returns true. Handles when a slot is
% just a single letter, or empty and is not valid.
row_to_slots([], Slot, [Slot]) :-
    length(Slot, L),
    L > 1.

% Second base case when the row is empty, returns true and ends
row_to_slots([], _, _).
    
% Main recursive case
% Handles the creation of new slots, the end of current slots, and validating
% the length of current slots
row_to_slots([H|Row], Slot, Slots) :-
    
    % If char is '#' end of Slot, add slot to Slots dependent on length
    ( H == '#' 
    ->  length(Slot, L),
        
        % If slot is longer then 1, add current slot to Slots and call again
        (L > 1
        ->  Slots = [Slot|Tail],
            row_to_slots(Row, [], Tail)
        
        % Else dont add current slot to Slots and call again
        ; row_to_slots(Row, [], Slots)
        )
    
    % else add current empty char to current Slot and call again
    ;   append(Slot, [H], NewSlot),
        row_to_slots(Row, NewSlot, Slots)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Solve Puzzle                                %
% This section takes to word slots, and takes the slot that can fit the least  %
%    amount of words, and tries every word in the slot, to try to solve the    %
%        if not solution at the choice point, backtracks and tries again       %

% solve_puzzle(+Wordslots, +Wordlist)
% Here is where we solve the puzzle, We have flatten which is
% The list of empty slots, and wordlist, the list of words to fit in
% the empty slot
% Returns true if puzzle has a solution
solve_puzzle([],_).
solve_puzzle(WordSlots, WordList) :-
    
    % find slot with least words that fit
    find_slot(WordSlots, WordList, Slot),

    %Find word that fits in Slot with least other slots it can fit in
    find_word(Slot, WordList, Word),

    % Remove word and slot, from wordlist and slotlist
    delete(Slot, WordSlots, NewWordSlots),
    delete(Word, WordList, NewWordList),

    solve_puzzle(NewWordSlots, NewWordList).
    % Recall solve_puzzle
    
% Initial find_slot/3 call
% gets num_word_fit of first slot in slotlist to assign initial bestcount to
% for secondary find_slot/5 call
find_slot([CurBestSlot|WordSlots], WordList, FinalBestSlot) :-
    num_words_fit(CurBestSlot, WordList, CurBestFit),
    find_slot(WordSlots, WordList, CurBestSlot, CurBestFit, FinalBestSlot).

% Main find_slot/5 call
% Has a currentslot to get num_words_fit, compares against bestSlot
% If currentSlot fits less words then bestSlot, currentSlot becomes bestSlot
% Predicate is recursively called until WordSlots is empty, i.e all slots tested
find_slot([],_,FinalBestSlot,_,FinalBestSlot).
find_slot([NewSlot|WordSlots], WordList, CurBestSlot, CurBestFit, FinalBestSlot):-
    num_words_fit(NewSlot, WordList, NewFit),
    (NewFit < CurBestFit
    ->  find_slot(WordSlots, WordList, NewSlot, NewFit, FinalBestSlot)
    ; find_slot(WordSlots, WordList, CurBestSlot, CurBestFit, FinalBestSlot)
    ).
    
       
% num_words_fit/3
% Recursive predicate that takes (+Slot, +WordList, -Count)
% iterates through wordlist and if CurWord fits in slot increments count
num_words_fit(_,[],0).
num_words_fit(Slot, [CurWord|Wordlist], Num) :-
    num_words_fit(Slot, Wordlist, Num1),
    (word_fit(Slot, CurWord)
    ->  Num is Num1 + 1
    ;   Num is Num1
    ).

% find_word(+Slot, +WordList, -Word)
% Takes a slot and wordlist and returns first word that fits in the slot
% Unifies the slot with the word to 'fill in the puzzle'
find_word(Slot, WordList, Word):-
    word_fit(WordList, Slot, WordFit),
    member(Word, WordFit),
    Slot = Word.

% word_fit(+WordList, +Slot, -WordFit)
% Takes a WordList and a slot and recursively calls itself comparing the 
% current word to the slot. If the word fits gets appended to Wordfit array
% Returns WordFit array with all words in WordList that fit in Slot
word_fit([],_,[]).
word_fit([H|WordList], Slot, WordFit) :-
    copy_term(Slot, DupSlot),
    word_fit(WordList, DupSlot, NewWordFit),
    (word_fit(DupSlot, H)
    ->  append([H], NewWordFit, WordFit)
    ; WordFit = NewWordFit
    ).
    
% word_fit/2
% Checks if TestSlot can be unified with TestWord
% Duplicates the terms, otherwise I was running into issues where they were
% being unified, when I didnt want them to be
word_fit(TestSlot, TestWord) :-
    copy_term(TestSlot, DupSlot),
    copy_term(TestWord, DupWord),
    DupSlot = DupWord.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Other helper functions                            %

% combine_Lists(+List1, +List2, -Output)
% Combine_lists take 2 lists as arguments and appends all the items in them
% indivdually, to return the two lists combined. If we just used append
% it doesnt keep the second list as nested and flattens everything.
combine_lists(Output, [], Output).
combine_lists(List1, [H|List2], Output) :-
    append(List1, [H], NewList1),
    combine_lists(NewList1, List2, Output).
    
% delete(+Item, +List, -Output)
% takes an item and a list, and returns the output list. Which is the original
% list with the first instance of item removed.
delete( _, [], []).
delete( R, [R|T], T).
delete( R, [H|T], [H|T2]) :-
    H \== R,
    delete( R, T, T2).