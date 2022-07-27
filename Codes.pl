■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■ Prolog Syntax Examples ■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

Note:   3+5 = 8.
	3+5 =:= 8.
	8 = 8.
	X = Y.
        3+5 is 8.   ('is' calculates only its right hand side)
	0 is 5*0.
	8 is 3+5.

X is 3/5.

X is 3+2.

X is +(3,2).

X is 5-3-1.

X is -(5,3,1).   % --error  -(-(5,3), 1)

X is -(-(5,3), 1).

X is 3 mod 5.

X is mod(3, 5).

Y is -(*(+(3,2),4),1).   % 3 + 2 * 4 - 1

X is 3 + 5, X = 8.

X is 3 + 5, X = 2.

X is f(3+5).   % --error

3 + 5 is X.

5 is 3+2.

3+2 is 5.

is(Y,^(2,2)).  % don't type space between 'is' and bracket

2 ** 3  =:=  3 + 5.

2 ** 3  =  3 + 5.

2 ** 3  =  X.

*(2,3) = 2*3.

X = Y.

sum(Y) = sum(2 + 3 + 4 + 15).

X = 3, C is X*X*X.

X = 3, X*X*X = C.

(X > 3) = (4 > 3).

(X > 3) = (4 > Y).

X = 2, X < 3.

5+3 > 4.

5+3 < 4.

2+1 >= 3.

2*2 =< 6.

5+3 >= 4-1.

0<3 ; fail.

0<3, fail.

0<3 ; !.

0<3, !.

0>3, !.

0>3 ; !.

X=5+4, X > 3-1.

X=5+4, X < 3-1.

foo(X|3) = foo(4,Y).

foo(X|3) = foo(4|Y).

B = C, A = B, B = 1, C = 2.

B = C, A = B, B = 1, C = 1.

Term1 = B, S = abc(Term1, Term2), B = abc, Term2=g(B,B,xyz).

12 > X.  (Also Possible With:  >, <, =<, >=  Which gives an instantiation error)


Errors:
	  3 + 4.

	  3 - 4.

	  3 * 4.

	  3 / 4.

	  3 mod 4.


?-  fred%nezar(mary).

?-  a*b+c/c-d   =   -(+(*(a,b), /(c,c)), d).

=============================================

% Sister relationship

male(albert).
male(edward).

female(alice).
female(victoria).

parents(edward, victoria, albert).
parents(alice, victoria, albert).

sister_of(X, Y) :- female(X), parents(X, M, F), parents(Y, M, F).


?- sister_of(alice, edward).
?- sister_of(alice, X).
?- sister_of(X, Y).

=============================================

// theif

thief(john).
likes(mary, chocolate).
likes(mary, wine).
likes(john, X) :- likes(X, wine).

% A person may steal something if the person is a thief and the person likes the thing.
may_steal(P, T) :- thief(P), likes(P, T).


?- may_steal(john, T).

=============================================

// family relationships

father(X, Y)    /* X is the father of Y */
mother(X, Y)    /* X is the mother of Y */
male(X)         /* X is male */
female(X)       /* x is female */
parent(X, Y)    /* X is a parent of Y */
diff(X, Y)      /* X and Y are different */

% X is a mother
is_mother(Mum) :- mother(Mum, Child).

% X is a father
is_father(Dad) :- father(Dad, Child).

% X is a son
is_son(Son) :- parent(Par, Son), male(Son).

% X is a sister of Y
sister_of(Sis, Pers) :-
     parent(Par, Sis), parent(Par, Pers),
     female(Sis),
     diff(Sis, Pers).

% X is a grandfather of Y
grandpa_of(Gpa, X) :- parent(Par, X), father(Gpa, Par).

% X is a sibling of Y
sibling(S1, S2) :- parent(Par, S1), parent(Par, S2), diff(S1, S2).  

=============================================

// structures

owns(john, book(wuthering_heights, author(emily, bronte), 3129)).

?- owns(john, book(X, author(Y, bronte), 3129)).
?- owns(john, book(_, author(_, bronte))).
?- owns(john, book(_, author(_, bronte), 3129)).

=============================================

// equality

equal(X, Y) :- X = Y.

Or

equal(X, X).


?- equal(5, 5).
?- equal(5, X).
?- equal(Y, X).

=============================================

► State whether the following goals would succeed, and which variables, if any, would be instantiated to what values:

pilots(A, london) = pilots(london, paris).

point(X, Y, Z) = point(X1, Y1, Z1).

letter(C) = word(letter).

noun(alpha) = alpha.

'student' = student.

"student" = student.

"student" = 'student'.

'will-smith' = will-smith.

f(X, X) = f(a, b).

f(X, a(b, d)) = f(Z, a(Z, c)).

policeman = policeman.

paper = penicl.

1066 = 1066.

rides(student, bicycle) = rides(student, X).

a(b, C, d(e, F, g(h, i, J))) = a(B, c, d(E, f, g(H, i, j))).

X=Y, X=1200.


■ Important Notes:
----------------------
►  Strings in Prolog are written in single quotes.
►  Terms written in double quotes are immediately converted to a list of ASCII character codes.

=============================================

All parts of Prolog, even Prolog programs themselves, are made up of constants, variables, and structures.

Prolog programs are built from 'terms'.

term: constant - variable - structure.

=============================================

% sum of list numbers

sumlist([], 0).
sumlist([H|T], N) :- sumlist(T, N1), N is N1+H.

?- sumlist([1,2,3], S).

=============================================

% sum of even numbers of a list

sumev([],0).
sumev([H|T], R) :- X is H mod 2, X\=0, sumev(T,R).
sumev([H|T], R) :- 0 is H mod 2, sumev(T,R1), R is H+R1.
 

?- sumev([2,4,3,6,8,12], X).


Or


even(X) :- 0 is X mod 2.

odd(X) :-              % using even
  Y is X + 1,
  even(Y).

sum_even([], 0).
sum_even([H|T], X) :- 
  even(H),
  sum_even(T, Y), 
  X is Y+H.

sum_even([H|T], X) :- 
  odd(H),
  sum_even(T, X).      % ignore the odd numbers

=============================================

% prince at a specific year

reigns(rhodri, 844, 878).
reigns(anarawd, 878, 916).
reigns(hywel_dda, 916, 950).
reigns(lago_ap_idwal, 950, 979).
reigns(hywel_ap_ieuaf, 979, 985).
reigns(cadwallon, 985, 986).
reigns(maredudd, 986, 999).

prince(X, Y) :- reigns(X, A, B), Y >= A, Y =< B.

?- prince(cadwallon, 986).
?- prince(rhodri, 1979).
?- prince(X, 900).
?- prince(X, 979).
?- prince(cadwallon, Year).

=============================================

% country density program

pop(usa, 203).
pop(india, 548).
pop(china, 800).
pop(brazil, 108).

area(usa, 3).
area(india, 1).
area(china, 4).
area(brazil, 3).

density(X, Y) :-
   pop(X, P),
   area(X, A),
   Y is P/A.

?- density(china, X).
?- density(turkey, X).
?- density(X, Y).

=============================================

% (and ',') example 

female(mary).

parent(C, M, F) :- mother(C, M), father(C, F).

mother(john, ann).
mother(mary, ann).

father(mary, fred).
father(john, fred).

?- female(mary), parent(mary, M, F), parent(john, M, F).

=============================================

sum(5).
sum(3).
sum(X + Y).  %  = sum( +(X, Y) ).

?- sum(2 + 3).  %  = sum( +(2, 3) ).
?- sum(X) = sum(5+Y).

=============================================

% Sum of two numbers

sum(X, Y, S) :- S is X+Y.

?- sum(5, 4, S).

=============================================

% Lists manipulation

[].
[a] = .(a, []).
[a, b, c] = .(a, .(b, .(c, []))).
[the, men, [like, to, fish]].
[a, V1, b, [X, Y]].

=============================================
   List                   Head          Tail
----------------------------------------------
 [a, b, c]                  a           [b, c]
    []                    (none)        (none)
[[the, cat], sat]       [the, cat]       [sat]
[the, [cat, sat]]          the         [[cat, sat]]
[the, [cat, sat], down]    the       [[cat, sat], down]
[X+Y, x+y]                 X+Y           [x+y]


% return head and tail of any given list

list([P1|P2], H, T) :-
    H = P1,
    T = P2.

?-  list([a, b, c] , H, T).

=============================================

% vertical bar notation

[X|Y], vertical bar '|'.

Be careful not to confuse the vertical bar '|' with:
  -  the digit  "1"
  -  the letter "I"
  -  the letter "l"

=============================================

% don't care variable '_'

p([1,2,3]).
p([the, cat, sat, [on, the, mat]]).

?- p([X|Y]).
?- p([_,_,_,[_|X]]).

=============================================

% Mixed lists matching examples

[_|_] = [a,b,c].

[a,b,c] = [_|_].

[a,b,c] = [_].

[_] = [a,b,c].

[X, Y, Z] = [john, likes, fish].

[cat] = [X|Y].

[X|Y] = [cat].

[X, Y|Z] = [mary, likes, wine].

[[the, Y]|Z] = [[X, hare], [is, here]].

[golden|T] = [golden, norfolk].

[golden|norfolk] = [golden, T].

[X,Y] = [a|b].

[vale, horse] = [horse, X].

[white|Q] = [P|horse].

L = [1,2|Y], Y=3.

L = [1,2|Y], Y=[3].

[a,X] = [a,b,c,d].

[a,X] = [a,[b,c,d]].

[_,_|_] = [a,b,c,d].

[X,X|Y] = [a,b,c,d].

[_] = [].

[_] = [a].

[_] = [a,b].

[_|_] = [].

[_|_] = [a].

=============================================

% check if an element is a member of a list

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

?-  member(d, [a, b, c, d, e, f, g]).
?-  member(d, [a, b, c, d, e, f, d, g]).
?-  member(2, [3, a, 4, f]).
?-  member(clygate, [currag_tip, music_star, park_mill, portland]).
?-  member(X, [1,2,3,4]).

=============================================

Important: avoid circular definitions.

Example:
-----------
parent(X, Y) :- child(Y, X).
child(A, B)  :- parent(B, A).

=============================================

♦ It is recommended to put 'facts' before 'rules' whenever possible.

mother(X, X).

person(adam).
person(X) :- person(Y), mother(X, Y).

?- person(X)


■ This program will loop!

=============================================

This program causes an infinite loop with 'islist(X)':

islist([_|X]) :- islist(X).
islist([]).

?- islist([a, b, c, d]).
?- islist([]).
?- islist(f(1, 2, 3)).
?- islist(X).

This also causes an infinite loop with 'islist(X)', starting from the second alternative solution: 

islist([]).
islist([_|X]) :- islist(X).

?- islist([a, b, c, d]).
?- islist([]).
?- islist(f(1, 2, 3)).
?- islist(X).

=============================================

% list lengths

Method #1:
listlen([], 0).
listlen([_|T], L) :- listlen(T, NL), L is NL+1.

?- listlen([1,2,3], L).


Method #2:
listlen([], A, A).
listlen([_|T], A, L) :- NA is A+1, listlen(T, NA, L).

?- listlen([1,2,3], 0, L).


Method #3:
listlen(List, L) :- listlenacc(List, 0, L).
listlenacc([], A, A).
listlenacc([_|T], A, L) :- NA is A+1, listlenacc(T, NA, L).

?- listlen([1,2,3], L).

=============================================

% last element in a list

my_last(L, [L]).
my_last(X, [_|L]) :- my_last(X,L).

?- my_last(LE, [1,2,3,4]).

=============================================

% reverse list

Method #1:
rev_acc([], Acc, Acc).
rev_acc([H|T], Acc, Rev) :- rev_acc(T, [H|Acc], Rev).


?- rev_acc([1,2,3], 0, Rev).
?- rev_acc([1,2,3], [], Rev).


Method #2:
reverse(List, Rev) :- rev_acc(List, [], Rev).

rev_acc([], Acc, Acc).
rev_acc([H|T], Acc, Rev) :- rev_acc(T, [H|Acc], Rev).

?- reverse([1,2,3], Rev).


Method #3:
my_reverse :-
   write('Enter a list: '),
   read(X),
   revacc(X, [], Y),
   write(Y),
   nl.
   
revacc([], A, A).
revacc([H|T], A, Y) :- revacc(T, [H|A], Y).

% ?- my_reverse.

---------------------------------

% reverse list - Using "Difference List"

reverse(X,Y) :- rev(X, Y-[]).

rev([], X-X).
rev([X|Y], Z-W) :- rev(Y, Z-[X|W]).


?- reverse([1,2,3], Rev).

=============================================

- This version just tests the first part of the list, rather than checking whether the last tail is [].

- This is not as strong as a test as 'islist' mentioned previously, but it will not loop if the argument is a variable.


weak_islist([]).
weak_islist([_|_]).

?- weak_islist([1,2,3]).
?- weak_islist(X).

=============================================

% loop with islist(X) as well:

islist([_|B]) :- islist(B).
islist([]).

=============================================

% Removes the element X from list L and returns the result in M

removeEl(_, [], []).  % returns the same list if the element doesn't exist
removeEl(X, [X|L], L).  % if the element is in the head
removeEl(X, [H|T], [H|L]) :- removeEl(X, T, L).  %  else recurse

?- removeEl(1, [1,2,3], L).
?- removeEl(1, [2,4,1,9,1], L).  % 3 solutions (remove first element - remove last element - return the list as it is)

=============================================

% Checks whether X and Y are two consecutive elements of the list L

nextto(X, Y, [X, Y|_]).
nextto(X, Y, [_|T]) :- nextto(X, Y, T).


?- nextto(1,2, [1,2,3]).
?- nextto(1,4, [1,2,3]).
?- nextto(1,X, [1,2,3]).

===================================================

% Elimiate 'consecutive' duplicates of list elements

compress([], []).
compress([X,X], [X]).
compress([X,Y], [X,Y]) :- X \= Y.
compress([X,X|Xs], Zs) :- compress([X|Xs], Zs).
compress([X,Y|Ys], [X|Zs]) :- X \= Y, compress([Y|Ys], Zs).

?- compress([1,1,2,3,4,4,5,4,6,5], L).  returns L = [1,2,3,4,5,4,6,5].

===================================================

% Delete all occurrences of an element

delete_all(_, [], []).
delete_all(H, [H|T], R) :- delete_all(H, T, R), !.
delete_all(X, [H|T], [H|R]) :- delete_all(X, T, R).


?‐  delete_all(a, [a,b,c,a,d,a], X).
?‐  delete_all(a, [b,c,d], X).

===================================================

% Remove all duplicated elements in a list

remove_duplicates([],[]).
remove_duplicates([H|T], L) :- member(H,T), !, remove_duplicates(T, L).
remove_duplicates([H|T], [H|L]) :- remove_duplicates(T, L).


%?- remove_duplicates([a, b, a, c, d, d], List).  List = [b, a, c, d]

===================================================

% Flatten list - Normal Way

flattent([],[]).
flattent([H|T], L3) :-
    flattent(H, L1),
    flattent(T, L2),
    append(L1, L2, L3).

flattent(X, [X]).


?- flattent([[a, b], [c, d, [e, f], g], h, [i, [k] ] ], X).

------------------------

% Flatten list - Difference List

flatt(X, Y) :- flatpair(X, Y-[]),

flatpair([], L-L).
flatpair([H|T], L1-L3) :- flatpair(H, L1-L2), flatpair(T, L2-L3).
flatpair(X, [X|Z]-Z).


?- flatt([[a, b], [c, d, [e, f], g], h, [i, [k] ] ], X).

===================================================

% program to detect whether a given list is a set or not

set([]).
set([H|T]):- \+member(H,T),set(T).


?- set([a,b,c,c,d]).
?- set([a,b,c]).
?- set([a,[b,a],c]).
?- set([]).

===================================================

% split(List, Positives, Negatives).

split([], [], []).

% Positives - into the second argument
split([Head|Tail], [Head|List1], List2) :-
    Head >= 0,
    split(Tail, List1, List2).

% Negatives - into the third argument
split([Head|Tail], List1, [Head|List2]) :-
    Head < 0,
    split(Tail, List1, List2).


?- split([3, -5, 5, 0, -2], Positives, Negatives).

===================================================

% Padovan sequence program

/*
Padovan:
    Pad(0) = 1
    Pad(1) = 1
    Pad(2) = 1
    Pad(n) = Pad(n-2) + Pad(n-3) for n>2
*/

------Code------

pad(0, 1).
pad(1, 1).
pad(2, 1).

pad(N, X) :-
    N > 2,
    N1 is N-2,
    N2 is N-3,
    pad(N1, A),
    pad(N2, B),
    X is A + B.

Or

pad(0, 1) :- !.
pad(1, 1) :- !.
pad(2, 1) :- !.

pad(N, Result) :-
    N1 is N-2,
    N2 is N-3,
    pad(N1, Result1),
    pad(N2, Result2),
    Result is Result1 + Result2.


?- pad(0,X).
X = 1.

?- pad(10,X).
X = 12.

?- pad(29,X).
X = 2513.

==================================================

% Padovan series program

/*
Fibonacci:
    Fib(0) = 0
    Fib(1) = 1
    Fib(n) = Fib(n-1) + Fib(n-2) for n>1.
*/

------Code------

fib(0, 1) :- !.
fib(1, 1) :- !.

fib(N, Result) :-
   N1 is N - 1,
   N2 is N - 2,
   fib(N1, Result1),
   fib(N2, Result2),
   Result is Result1 + Result2.

?- fib(5, Result).

===================================================

% delete an element evert nth step

delete_nth(L, C, R) :- delete_nth(L, C, 1, R).

delete_nth([], _, _, []).
delete_nth([_|T], C, C, T1) :- !, delete_nth(T, C, 1, T1).
delete_nth([H|T], N, C, [H|T1]) :- C<N, C1 is C+1, delete_nth(T, N, C1, T1).

Or


delete_nth(L1,N,L2) :- delete_nth(L1,N,L2,N).

delete_nth([],_,[],_).
delete_nth([_|Xs],N,Ys,1) :- delete_nth(Xs,N,Ys,N).
delete_nth([X|Xs],N,[X|Ys],K) :- K > 1, K1 is K - 1, delete_nth(Xs,N,Ys,K1).

===================================================

% List pretty print

spaces(0) :- !.
spaces(I) :- write(' '), J is I-1, spaces(J).

pp(X) :- pp(X, 0).

pp([H|T], I) :- !, J is I+3, pp(H, J), ppx(T, J).
pp(H, I) :- spaces(I), write(H), nl.

ppx([], _).
ppx([H|T], I) :- pp(H, I), ppx(T, I).


?- pp([1, 2, [3,4], 5).

===================================================

% Convert Fahrenheit to Celsius
% Celsius = (Fahrenheit - 32) / 1.8

program :-
   write("Enter Fahrenheit degree: "),
   read(X),
   Y is ((X - 32) / 1.8),
   write("The equivalent Celsius: "),
   write(Y),
   nl.

?- program.

===================================================

% number times itself

program :-
    write("Enter an integer number: "),
    read(X),
    X1 is X*X,
    write(X1),
    nl,
    prompt_user.
    
    
prompt_user :-
    write("\nEnter 'y' (more), 'n' (exit): "),
    read(X),
    ans(X).
    
ans('y') :- program.
ans('n') :- fail.


?- program.

===================================================

% prime factors program

prime_factors(N, L) :- N > 0, prime_factors(N, 2, L).

prime_factors(1, F, []) :- !.
prime_factors(N, F, [F|L]) :-
     R is N // F,
     N =:= R * F,
     !,
     prime_factors(R, F, L).
     
prime_factors(N, F, L) :-
     next_factor( N, F, NF),
     prime_factors(N, NF, L).
     
next_factor(N, 2, 3):- !.
next_factor(N, F, NF) :-
     F *F < N, !,
     NF is F + 2.

next_factor(N, _, N).

===================================================

% Change uppercase to lowercase and vice versa

change_case :-
   write("Enter a character: "),
   read(X),
   atom_codes(X, XL),
   convert(XL).

% Handle uppercase characters (65-90)
convert([H|_]) :-
   H =< 90,
   Z is H+32,
   atom_codes(X, [Z]),
   write(X).
   
% Handle lowercase characters (97-122)
convert([H|_]) :-
   H >= 97,
   Z is H-32,
   atom_codes(X, [Z]),
   write(X).

===================================================

% factorial of a number (infinite loop with alternative solution)

fact(1,1).
fact(N,F):- M is N-1 , fact(M,Z), F is Z*N.

?- fact(3, F).

------------------------

% using accumulator (infinite loop with alternative solution)

fact_acc(N, F) :- fact_acc(N, 1, F).

fact_acc(0, Acc, Acc).
fact_acc(N, Acc0, F) :-
    Acc is Acc0 * N,
	N1 is N-1,
	fact_acc(N1, Acc, F).

?- fact_acc(3, F).

------------------------

% factorial of a number (without alternative solutions - using cut '!')

fact(1,1).
fact(N,F):- M is N-1 , fact(M,Z), F is Z*N, !.

Or

fact(1,1).
fact(N,F):- N>1, M is N-1 , fact(M,Z), F is Z*N.


?- fact(3, F).

------------------------

% using accumulator (without alternative solutions - using cut '!')

fact_acc(0, A, A).
fact_acc(N, A, F) :-
    A1 is A*N,
    N1 is N-1,
    fact_acc(N1, A1, F),
    !.

?- fact_acc(3, F).

===================================================

% Mapping Example:

change(you, i).
change(are, [am, not]).
change(french, german).
change(do, no).
change(X, X).    /* this is the 'catchall' */

alter([], []).
alter([H1|T1], [H2|T2]) :- change(H1, H2), alter(T1, T2).


?- alter([you, are, a, computer], Ans).


▬ Notice that we have treated the phrase 'am not' as a list "[am, not]", so that it occupies only one argument of the fact.


♦ Note that you can allow only one solution using cut:

    alter([H1|T1], [H2|T2]) :- change(H1, H2), alter(T1, T2), !.

=============================================

% fuel consumption example

fuel_consumed(waster, [3.1, 10.4, 15.9, 10.3]).
fuel_consumed(guzzler, [3.2, 9.9, 13.0, 11.6]).
fuel_consumed(prodigal, [2.8, 9.8, 13.1, 10.4]).

equal_or_better_consumption(Good, Bad) :-
   Threshold is (Good + Bad) / 40,
   Worst is Bad + Threshold,
   Good < Worst.

?- equal_or_better_consumption(10.5, 10.7).
?- equal_or_better_consumption(10.7, 10.5).
?- equal_or_better_consumption(10.1, 10.7).
?- equal_or_better_consumption(10.7, 10.1).
?- equal_or_better_consumption(X, 10.7).   % -- error
?- equal_or_better_consumption(10.1, X).   % -- error

prefer(Car1, Car2) :-
    fuel_consumed(Car1, Con1),
    fuel_consumed(Car2, Con2),
    always_better(Con1, Con2).

always_better([], []).
always_better([Con1|T1], [Con2|T2]) :-
    equal_or_better_consumption(Con1, Con2),
    always_better(T1, T2).


?- prefer(waster, guzzler).
?- prefer(prodigal, waster).
?- prefer(X, guzzler).
?- prefer(waster, X).
?- prefer(X, Y).

=============================================

% ASCII Codes

?- atom_codes("Mohamed", X).
?- atom_codes(Mohamed, X).     % -- error
?- atom_codes(mohamed, X).
?- atom_codes('mohamed', X).
?- atom_codes(X, [109, 111, 104, 97, 109, 101, 100]).


?- name("Mohamed", X).
?- name(Mohamed, X).   % -- error
?- name(mohamed, X).
?- name('mohamed', X).
?- name(X, [109, 111, 104, 97, 109, 101, 100]).

=============================================

% X is alphabetically less than Y.

% aless == atom less
% alphabetically: (i.e.) a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z


aless(X, Y) :-
	atom_codes(X, XL),
	atom_codes(Y, YL),
	alessx(XL, YL).

alessx([], [_|_]).
alessx([X|_], [Y|_]) :- X < Y.
alessx([H|X], [H|Y]) :- alessx(X, Y).


?- aless(avocado, clergyman).
?- aless(windmill, motorcar).
?- aless(picture, picture).
?- aless(book, bookbinder).
?- aless(bookbinder, book).
?- aless(lazy, leather).
?- aless(apple, apple).
?- aless(alphabetic, alp).
?- aless(X, Y).   % -- error
?- atom_codes(alp, [97, 108, 112]).
?- atom_codes(alp, X).
?- atom_codes(X, [97, 108, 112]).
?- atom_codes(X, Y).  % -- error

=============================================

% append two lists (Normal Way)

append([], L, L).
append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).


?- append([a, b, c], [2, 1], [a, b, c, 2, 1]).
?- append([a, b, c], [2, 1], X).
?- append(X, [2, 1], [a, b, c, 2, 1]).
?- append([a, b, c], X, [a, b, c, 2, 1]).
?- append(X, Y, [a, b, c, 2, 1]).
?- append(X, Y, Z).
?- append([a, b, c], [3, 2, 1], [a, b, c, 3, 2, 1]).
?- append([alpha, beta], [gama, delta], X).
?- append(X, [b, c, d], [a, b, c, d]).

---------------------

% append using 'Difference List'

diff_append(OpenList, Ho, L2) :- Ho = L2.

?- List=[a,b,c|Ho], diff_append(List, Ho, [d,e]).

Or: diff_append(OpenList, Ho, Ho).

---------------------

append(A-B, B-C, A-C).

?- append([1,2,3|T]-T, [4,5,6|W]-W, DL).


♦ You can get to the concrete answer by "filling" the hole with an empty list in the last parameter:

?- append([1,2,3|T]-T, [4,5,6|W]-W, L-[]).


---------------------

% append problem with 'cut'

append([], L, L) :- !.
append([H|L1], L2, [H|L3]) :- append(L1, L2, L3).


?- append(X, [b, c, d], [a, b, c, d]).

=============================================

% Bike parts - Using 'append'

basicpart(rim).
basicpart(spoke).
basicpart(rearframe).
basicpart(handles).
basicpart(gears).
basicpart(bolt).
basicpart(nut).
basicpart(fork).

assembly(bike, [wheel, wheel, frame]).
assembly(wheel, [spoke, rim, hub]).
assembly(frame, [rearframe, frontframe]).
assembly(hub, [gears, axle]).
assembly(axle, [bolt, nut]).
assembly(frontframe, [fork, handles]).

partosf(X, [X]) :- basicpart(X).
partosf(X, P) :- 
    assembly(X, Subparts),
    partsoflist(Subparts, P).

partsoflist([], []).
partsoflist([H|T], P) :-
    partosf(H, Headparts),
    partsoflist(T, Tailparts),
    append(Headparts, Tailparts, P).


?- partsof(bike, Parts).
?- partsof(wheel, Parts).

---------------------

% Bike Parts - Using 'Accumulator'

partsof(X,P) :- partsacc(X, [] ,P).

partsacc(X, A, [X|A]) :- basicpart(X).

partsacc(X, A, P) :-
	assembly(X, Subparts),
	partsacclist(Subparts, A, P).

partsacclist([], A, A).
partsacclist([P|Tail], A, Total):-
	partsacc(P, A, Headparts),
	partsacclist(Tail, Headparts, Total).


% partsacc(X, A, P): parts of X, when added to A, gives P.


?- partsof(bike, Parts).
?- partsof(wheel, Parts).


Or (My style)

partsof(X, A, [X|A]) :- basicpart(X).
partsof(X, A, P) :- 
    assembly(X, Subparts),
    partsoflist(Subparts, A, P).

partsoflist([], A, A).
partsoflist([H|T], A, P) :-
    partsof(H, A, Headparts),
    partsoflist(T, Headparts, P).

---------------------

% Bike Parts - Using 'Difference List'

partsof(X,P) :- partsdif(X, Hole, P), Hole=[].

partsdif(X, Hole, [X|Hole]) :- basicpart(X).
partsdif(X, Hole, P) :-
	assembly(X, Subparts),
	partsdiflist(Subparts, Hole, P).

partsdiflist([], Hole, Hole).
partsdiflist([P|Tail], Hole, Total) :- 
	partsdif(P, Headparts, Total),
	partsdiflist(Tail, Hole, Headparts).


?- partsof(bike, Parts).
?- partsof(wheel, Parts).

=============================================

% components of a phrase - Using 'append'

basicpart(verb_phrase).
basicpart(the).
basicpart(apple).
basicpart(fruit).

assembly(sentence, [noun_phrase, verb_phrase]).
assembly(noun_phrase, [determiner, noun]).
assembly(determiner, [the]).
assembly(noun, [apple]).
assembly(noun, [fruit]).

partsof(X, [X]) :- basicpart(X).
partsof(X, P) :-
   assembly(X, Subparts),
   partsoflist(Subparts, P).

partsoflist([], []).
partsoflist([P|Tail], Total) :-
   partsof(P, Headparts),
   partsoflist(Tail, Tailparts),
   append(Headparts, Tailparts, Total).

=============================================

% Open List

?- List=[a,b,c|Ho], Ho=[d,e].    % 'Proper list'
?- List=[a,b,c|Ho], Ho=[d,e|Y].  % 'Open list' again

------------------------

% Open_append

open_append([H1,H2,H3|Hole], L2) :- Hole = L2.

?- List=[a,b,c|Ho], open_append(List, [d,e]).


Or:  open_append([H1,H2,H3|Hole], Hole).

-------------------------

% Difference List

diff_append(OpenList, Ho, L2) :- Ho = L2.

?- List=[a,b,c|Ho], diff_append(List, Ho, [d,e]).


Or: diff_append(OpenList, Ho, Ho).

--------

diff_append(OpenList-Hole, L2) :- Hole=L2.

?- DList=[a,b,c|Ho]-Ho, diff_append(DList, [d,e]).


Or: diff_append(OpenList-Hole, Hole).

-----------------------------

% True/False

DL = [a, b | Var], Var = [c].

DL = [a, b | Var1], Var1 = [c | Var2], Var2 = [d].

DL = .(a, .(b, Var)), Var = .(c, []).

DL = '·'(a, '·'(b, Var)), Var = '·'(c, []).

DL = '[|]'(a, '[|]'(b, Var)), Var = '[|]'(c, []).

=============================================

% Pair example

pair(X, Y) :- boy(X), girl(Y).

boy(john).
boy(marmaduke).
boy(bertram).
boy(charles).
girl(griselda).
girl(ermitrude).
girl(brunhilda).

?- pair(X, Y).    % 3 boys * 4 girls  =  12 solutions

=========================================================

% sum from 1 to a specific given number

sum_to(1,1).
sum_to(N, S) :- NA is N-1, sum_to(NA, SA), S is SA+N.

?- sum_to(3, N).

-----------------------------

% Another way

sum_to(1,1).
sum_to(N, S) :-
	N > 1,   % \+(N =< 1)
	NA is N-1,
	sum_to(NA, SA),
	S is SA+N.

?- sum_to(3, N).

=========================================================

% [1] Cut benifits: program will run faster & occupy less memory.


[2] Cut Usage:
------------------

1) Confirming the Choice of a Rule (use 1)

sum_to(1,1) :- !.
sum_to(N, S) :- NA is N-1, sum_to(NA, SA), S is SA+N.

?- sum_to(3, N).

=========================================================

2) 'Cut-fail' Combination (use 2)

% cut-fail example     **fail: return False + Backtrack**

foreign(mohand).
foreign(fahd).

grade3_fees(X, Discount, Fees) :- foreign(X), !, fail.
grade3_fees(X, Discount, Fees) :- Fees is 200-Discount.

?- grade3_fees(ahmed, 50, Fees).
?- grade3_fees(fahd, 50, Fees).


OR


foreign(mohand).
foreign(fahd).

grade3_fees(X, Discount, Fees) :- \+foreign(X), Fees is 200-Discount.

?- grade3_fees(ahmed, 50, Fees).
?- grade3_fees(fahd, 50, Fees).

=========================================================

% infinite numbers, starting from zero

is_integer(0).
is_integer(X) :- is_integer(Y), X is Y+1.

?- is_integer(X).

-------------------- OR --------------------

% Another way of implementation

is_integer(0).
is_integer(X) :- is_integer(Y), X is Y+1.

div(X, Y, R) :-
	is_integer(R),
	Temp is Y*R,
	Temp = X,
	!.


?- div(4, 2, R).

-------------------- OR --------------------

is_integer(0).
is_integer(X) :- is_integer(Y), X is Y+1.

divide(N1, N2, R) :-
    is_integer(R),
    Product1 is R * N2,
    Product2 is (R + 1) * N2,
    Product1 =< N1, Product2 > N1,
    !.

?- divide(4, 2, R).

=========================================================

3) Terminate a 'generate-test' (use 3)


% XO Game

%   (b(1,2,3,4,5,6,7,8,9),X,Y,Z).
line(b(X,Y,Z,_,_,_,_,_,_), X,Y,Z).
line(b(_,_,_,X,Y,Z,_,_,_), X,Y,Z).
line(b(_,_,_,_,_,_,X,Y,Z), X,Y,Z).
line(b(X,_,_,Y,_,_,Z,_,_), X,Y,Z).
line(b(_,X,_,_,Y,_,_,Z,_), X,Y,Z).
line(b(_,_,X,_,_,Y,_,_,Z), X,Y,Z).
line(b(X,_,_,_,Y,_,_,_,Z), X,Y,Z).
line(b(_,_,X,_,Y,_,Z,_,_), X,Y,Z).

threatening(x,x,e).
threatening(x,e,x).
threatening(e,x,x).

forced_move(Board) :- 
    line(Board, X,Y,Z),  % generate
    threatening(X,Y,Z),  % test
    !.


% ?- forced_move(b(x,x,e,e,o,x,o,e,e)).

              x  x  e
              e  o  x
              o  e  e

=========================================================

% Cut Problems (Only one solution is given, instead of many)

append([], X, X) :- !.
append([H1|T1], L2, [H1|L3]) :- append(T1, L2, L3).


?- append(X, Y, [a,b,c]).

=========================================================

*** Built-in Predicates ***

write(X).  % if X is uninstantiated a number like _231 will be displayed, which represents a variable

?-  X=5, write(X).
?-  write(X).
?-  write(2+3).
?-  write('2+3').

read(X).

mysum :- read(X), read(Y), Z is X+Y, write(Z).

?- mysum.

=========================================================

% Pretty Print

spaces(0) :- !.
spaces(I) :- write(' '), J is I-1, spaces(J).

pp(X) :- pp(X,0).

pp([H|T], I) :- !, J is I+3, pp(H,J), ppx(T,J).
pp(H,I) :- spaces(I), write(H), nl.

ppx([], _).
ppx([H|T], I) :- pp(H,I), ppx(T,I).


?- pp([1,[2], 3], 0).

=========================================================

event(1505, ['Euclid', translated, into, 'Latin']).
event(1510, ['Reuchlin-Pfefferkorn', controversy]).
event(1523, ['Christian', 'II', flees, from, 'Denmark']).

hello1(Event) :- read(Date), event(Date, Event).


?- hello1(X).

=========================================================

event(1505, ['Euclid', translated, into, 'Latin']).
event(1510, ['Reuchlin-Pfefferkorn', controversy]).
event(1523, ['Christian', 'II', flees, from, 'Denmark']).

spaces(0) :- !.
spaces(I) :- write(' '), J is I-1, spaces(J).

phh([]) :- nl.
phh([H|T]) :- write(H), spaces(1), phh(T).


%?- event(_, L), member('Denmark', L), phh(L).

Or

hello2 :-
    phh(['What', date, do, you, 'desire?']),
    read(D),
    event(D, S),
    phh(S).

%?- hello2.

=========================================================

% get_char(): gets a single character from the user.

isO :- get_char(X), checkO(X).

checkO('o') :- !.
checkO('O') :- !.

checkO(_) :- write("Opsss.....try again!"), isO.


?- isO.

=========================================================

isO :- get_char(X), checkO(X).

checkO(X) :- X='o' ; X='O', !.

checkO(_) :- write('Opsss...try again!'), isO.


?- isO.

=========================================================

% files

read_file :- get_char(C), show(C).

show('end_of_file') :- !.
show(C) :- put_char(C), read_file.

program :-
    open('d:\\a.txt', read, X),
    current_input(S),
    set_input(X),
    read_file,
    set_input(S),
    close(X).


?- program.


% get_char(X) reads a character from the keyboard.
% put_char(X) displays the instantiated character X on the screen. i.e. put_char(‘h’)

=========================================================

?-  get_char(X).
?-  get_char(a).
?-  get_char(3*5).     % -- error

--------

?-  put_char(X).        % -- error
?-  put_char(a).
?-  put_char('a').
?-  put_char("a").
?-  put_char(9).
?-  put_char(3*4).      % -- error
?-  put_char('3*4').    % -- error

=========================================================

% loop simulation

rep.
rep :- rep.

=========================================================

% loop example

rep.
rep :- rep.

run :-
   rep,
   write('Enter character: '),
   nl,
   read(42).

?-  run.


Note: you can use the built-in predicate: repeat.

   ?-  repeat.

=========================================================

rep.
rep :‐ rep.

run :‐
    rep,
    write('Try to guess the secret number'),
    nl,
    read(42),
    write("Congrats!"),
    nl,
    write('Sorry, wanted to say: Congrats!').

?-  run.

=========================================================

% type checker

check_line(OK) :-
    get_char(X),
    rest_line('\n', X, OK).

rest_line(_, '\n', yes) :- !.
rest_line(Last, Current, no) :-
    typing_error(Last, Current), !,
    get_char(New),
    rest_line(Current, New, _).

rest_line(_, Current, OK) :-
    get_char(New),
    rest_line(Current, New, OK).

typing_error('q', 'w').
typing_error('c', 'v').


?- check_line(X).
please could you enter your cvomments on the proposal

=========================================================

% Enhanced type checker: to correct errors

correct_line :-
     get_char(X),
     correct_rest_line('\n', X).

correct_rest_line(C, '\n') :- !,
     put_char(C), nl.

correct_rest_line(Last, Current) :-
     typing_correction(Last, Current, Corr), !,
     get_char(New),
     correct_rest_line(Corr, New).

correct_rest_line(Last, Current) :-
     put_char(Last),
     get_char(New),
     correct_rest_line(Current, New).

typing_correction('q', 'w', 'q').
typing_correction('c', 'v', 'c').

?- correct_line.
please could you enter your cvomments on the proposal

=========================================================

% Program that reads from a file (general form)

program :-
         open('d:\\a.txt', read, X),
         code_reading_from(X),
         close(X).

----------

% Program that writes to a file (general form)

program :-
         open('d:\\a.txt', write, X),
         code_writing_to(X),
         close(X).

code_reading_from & code_writing_to: represent the to be implemented 'reading/writing codes'

=========================================================

% Program that reads from a file (in more detail)

program :-
         open('d:\\a.txt', read, X),
         current_input(Stream),
         set_input(X),
         code_reading,
         close(X),
         set_input(Stream).

----------

% Program that writes to a file (in more detail)

program :-
         open('d:\\a.txt', write, X),
         current_output(Stream),
         set_output(X),
         code_writing,
         close(X),
         set_output(Stream).


code_reading & code_writing: represent the to be implemented 'reading/writing code'

-----------------------------------

% Changing/Setting the current input and ouput

♦ set_input(X)    -- for reading
♦ set_output(X)   -- for writing

% Saving the current input and output

♦ current_input(Stream)    -- for reading
♦ current_output(Stream)   -- for writing

=========================================================

% read from a file (d:\\input.txt)

read_file :- get_char(C), show(C).

show('end_of_file') :- !.
show(C) :- put_char(C), read_file.

program :-
    open("d:\\input.txt", read, X),
    current_input(S),
    set_input(X),
    read_file,
    close(X),
    set_input(S).


?-  program.

-----------------------------------

% write to a file (d:\\a.txt)

write_file :- get_char(C), show(C).

show('.') :- put_char('.'), !.
show(C) :- put_char(C), write_file.

program :-
    open("d:\\a.txt", write, X),
    current_output(S),
    set_output(X),
    write_file,
    close(X),
    set_output(S).


?-  program.

-----------------------------------

% Program to count the words of a given file

count(X) :- get_char(C), count(C, X).

count('end_of_file', X) :- write(X), !.
count(' ', X) :- !, NX is X+1, count(NX).
count(_, X) :- count(X).

program :-
    open("d:\\input.txt", read, X),
    current_input(S),
    set_input(X),
    count(1),
    close(X),
    set_input(S).


?- program

-----------------------------------

% returns true if 'Atom' was found in the file 'F'

find(F, Atom) :-
   open(F, read, X),
   current_input(S),
   set_input(X),
   atom_codes(Atom, L),
   read_file(L,L),
   close(X),
   set_input(S).
   
read_file([], _).

read_file([H|T], L) :-
   get_char(C),
   C \= 'end_of_file',
   atom_codes(C, CL),
   comp(H, CL),
   read_file(T,L).
   
read_file([H|_], L) :-
   get_char(C),
   C \= 'end_of_file',
   atom_codes(C, CL),
   \+comp(H, CL),
   read_file(L,L).
   
comp(H, [H|_]).



?- find('d:\\find.txt', 'one').

=========================================================

% *** Built-in predicates ***

var/1: returns True for 'uninstantiated variables' only

?-  var(2).
?-  var(X).
?-  var(X+2).
?-  var(X+Y).
?-  var([X]).
?-  var([a]).
?-  X=2, var(X).
?-  X=Y, var(X).
?-  X=Y, var('X').

-------------------------

nonvar/1: returns True for both 'atoms or numbers'
nonvar/1: succeeds if it is not a variable, or already instantiated

?-  nonvar(2).
?-  nonvar(a).
?-  nonvar(X).
?-  nonvar([X]).
?-  nonvar([a]).
?-  nonvar(X-5).
?-  nonvar(X-a).
?-  nonvar(X+Y).
?-  X=2, nonvar(X).

-------------------------

atom/1: returns True for 'atoms only'
atom definition: a non-variable term with 0 arguments, and not a number

?-  atom(a).
?-  atom('a').
?-  atom("a").
?-  atom(2).
?-  atom(X).
?-  atom([]).
?-  atom([a,b]).
?-  atom(a+5).
?-  atom(X+Y).
?-  X=a, atom(X).

-------------------------

number/1: returns True for 'numbers only'

?-  number(2).
?-  number('3').
?-  number("3").
?-  number(5.46).
?-  number(a).
?-  number(X).
?-  number(5+10).
?-  number(X+10).
?-  X=2, number(X).
?-  X is 5+2, number(X).

-------------------------

integer/1: checks if the number is integer (Positive, Negative or Zero)

?-  integer(5).
?-  integer(+5).
?-  integer(-5).
?-  integer(0).
?-  integer(3.24).
?-  integer(a).
?-  integer(X).
?-  integer(0.0).
?-  integer('5').
?-  integer("5").
?-  X = 5, integer(X).

-------------------------

float/1: checks if the number is float (stands for a real number)

?-  float(5).
?-  float(3.24).
?-  float(+3.24).
?-  float(-3.24).
?-  float(0).
?-  float(0.0).
?-  float(a).
?-  float(X).

-------------------------

atomic/1: returns True for both 'atoms or numbers'
  
?-  atomic(2).    % Compare it with 'atom(2).'
?-  atomic(a).
?-  atomic(5+2).
?-  atomic('5+2').
?-  atomic(X).
?-  atomic('X').
?-  X=2, atomic(X).

-------------------------

compound(Term)/1: True if 'Term' is bound to a compound term

?-  compound(0).
?-  compound([]).
?-  compound([a]).
?-  compound(b(a)).
?-  compound('b(a)').
?-  compound(b).
?-  compound([X]).
?-  compound([X+Y]).

-------------------------

ground(X)/1: Succeeds if X does not contain any uninstantiated variables.
             Also checks inside compound terms.

?-  ground(X).
?-  ground(a(b,X)).
?-  ground('a(b,X)').
?-  ground(a).
?-  ground([a,b,c]).
?-  ground(15).
?-  ground(15+X).
?-  ground(15+'X').

-------------------------

listing: shows the structure of a specific predicate (name + arguments + body)

?-  listing(predicate's name).

likes(ahmed, cola).
likes(medo, cola).

likes(_, Y) :- likes(Y, cola).

?-  listing(likes).

-------------------------

asserta: adds a predicate to the database through the question mode

?-  asserta(likes(ahmed, mona)).

-------------------------

When using compound structures you can’t use a variable to check or make a functor.

?-  X=tree,  Y = X(maple).    **Syntax Error**

-------------------------

functor/3: returns the 'name of the predicate' + 'number of arguments (arity)'

?-  functor(f(a, b, g(Z)), F, N).
?-  functor(f(a, b, g(Z), F, N)).    %  Wrong Parentheses
?-  functor(a + b, F, N).            %  +(a, b).
?-  functor([a, b, c], F, N).        %  .(a, .(b, .(c, [])) )
?-  functor(apple, F, N).
?-  functor([a, b, c], '.', 3).
?-  functor([a, b, c], ., 2).
?-  functor([a, b, c], '[|]', 2).
?-  functor(t(f(X), a, T), Func, N).



copy(Old, New) :- functor(Old, F, N), functor(New, F, N).

?-  copy(sentence(np(n(john)), v(eats)), X).

np: noun_phrase
n: noun
v: verb_phrase

-------------------------

clause/2: returns the body of the predicate

?-  clause(predicat's name, X).


likes(ahmed, mona).
?-  clause(likes(ahmed, mona), X).

likes(ahmed, mona) :- likes(mona, cola).
?-  clause(likes(ahmed, mona), X).

-------------------------

arg/3: returns specific argument (true if A is the Nth argument in Term)

?-  arg(argument number, predicate, the result).


?-  arg(2, related(john, mother(jane)), X).
?-  arg(1, a+(b+c), X).
?-  arg(1, a+(b+c), a).
?-  arg(2, a+(b+c), X).     %  +(a, +(b, c))
?-  arg(2, a+(3-2)-5, X).   %  -(+(a,-(3,2)),5)
?-  arg(2, a+3, X).         %  +(a, 3)
?-  arg(2, [a], X).         %  .(a, [])
?-  arg(2, [a,b,c], X).     %  .(a, .(b, .(c, [])))
?-  arg(1, a+(b+c), b).     %  +(a, +(b,c))
?-  arg(2, t(t(X), []), A).


?-  functor(D, date, 3), arg(1, D, 11), arg(2, D, oct), arg(3, D, 2004).

-------------------------

 universal ( =.. )
 =../2 : decomposes a structure into a list of its components

?-  foo(a, b, c) =.. X.
?-  foo(a, b, c) =..X.
?-  foo(a, b, c) = .. X.   % error

?-  append([A|B], C, [A|D]) =.. L.

?-  [a, b, c, d] =.. L.    % .(a, .(b, .(c, .(d, []))))

?-  (a+b) =.. L.   % +(a, b).

?- T =.. [is_blue, sam, today].

?- f(2,3) =.. [F,N|Y], N1 is N*3, L=.. [F,N1|Y].

-------------------------

?-  atom_chars(apple, X).
?-  atom_chars(X, [a,p,p,l,e]).
?-  atom_chars(234, X).
?-  atom_chars(A, X).

-------------------------

?-  atom_codes(apple, X).
?-  atom_codes(apple, [97, 112, 112, 108, 101]).
?-  atom_codes(X, [97, 112, 112, 108, 101]).
?-  atom_codes([97, 112, 112, 108, 101], apple).

-------------------------

?-  number_chars(234, L).
?-  number_chars(2.35, L).
?-  number_chars(2.35+2, L).
?-  number_chars(A, L).
?-  number_chars(a, L).

-------------------------

findall/3:
           •  includes all duplicated solutions (as prolog finds them)
           •  infinite solutions (may never terminate!)
           •  most commonly used


?-  findall(X, member(X, [1,2,3,4]), Results).
?-  findall(X, (member(X, [1,2,3,4]), X>2), Results).
?-  findall(X/Y, (member(X, [1,2,3,4]), Y is X * X), Results).
?-  findall(Child, age(Child,Age), Results).

-------------------------

setof/3:
           •  no duplicates
           •  sorted results
           •  separate result for each possible instantiation of a variable


age(peter, 7).
age(ann, 5).
age(pat, 8).
age(tom, 5).
age(ann, 5).   % duplicate


?-  setof(Child, age(Child, Age), Results).
?-  setof(Age/Children, setof(Child, age(Child,Age), Children), AllResults).
?-  setof(Children/Age, setof(Child, age(Child,Age), Children), AllResults).
?-  setof(Child, Age^age(Child,Age), Results).
?-  setof(Child/Age, Age^age(Child,Age), Results).
?-  setof(Age, Child^age(Child,Age), Results).
?-  setof(Child/Age, age(Child, Age), Results).

-------------------------

begof/3: very much like setof/3 except it contains 'duplicates + not sorted results'

?-  bagof(Age/Children, bagof(Child, age(Child,Age), Children), AllResults).
?-  bagof(Children/Age, bagof(Child, age(Child,Age), Children), AllResults).
?-  bagof(Child, Age^age(Child,Age), Results).
?-  bagof(Age, Child^age(Child,Age), Results).
?-  bagof(Age, age(Child,Age), Results).  % Sorted results by Child name
?-  bagof(Child, age(Child,Age), Results). % Sorted results by Age
?-  bagof(Child/Age, age(Child, Age), Results).

-------------------------

% Main Differences!

?- findall(Child, age(Child, Age), Results).    %  Unsorted +  Duplicates
?- bagof(Child, age(Child,Age), Results).       %  Unsorted +  Duplicates
?- setof(Child, age(Child,Age), Results).       %  Sorted   +  No Duplicates


♦ findall/3  &  bagof/3:  Unsorted + Duplicates
♦ setof/3: Sorted + No Duplicates    الوحيدة اللي سورتد و مفهاش دابليكيتس

-------------------------

knows(jim, fred).
knows(alf, bert).

?-  setof([X,Y], knows(X,Y), Z).


--------------------- Other Programs ----------------------

% Algorithm
% while (n!=0): gcd(m,n, result) = gcd(n, m%n, result) ... etc

gcd(X, 0, X) :- !.
gcd(M, N, R) :- D is M mod N, gcd(N, D, R).

%?- gcd(30, 6, D). D=6
%?- gcd(15, 4, D). D=1

-----------------------------------------------------------

my_reverse :-
   write('Enter a list: '),
   read(X),
   revacc(X, [], Y),
   write(Y),
   nl.
   
revacc([], A, A).
revacc([H|T], A, Y) :- revacc(T, [H|A], Y).

% ?- my_reverse.

-----------------------------------------------------------

% A perfect number is a positive integer that is equal to the sum of its divisors
% However, the number itself is not included in the sum.
% a perfect number 6 = 1+2+3 while a non-perfect number 5 != 1+5.

perfect(X) :- perfect(X, 1, 0).

perfect(X, X, X) :- !.

perfect(X, Y, S) :-
     Y < X,
     Z is X mod Y,
     Z=0,
     NewS is S+Y,
     NewY is Y+1,
     perfect(X, NewY, NewS).
     
perfect(X, Y, S) :-
     Y < X,
     Z is X mod Y,
     Z\=0,
     NewY is Y+1,
     perfect(X, NewY, S).


?- perfect(6).
?- perfect(5).

-----------------------------------------------------------

% Check if it is a 'set' or not
% Set: no repeated elements!

set([]).
set([H|T]):- \+member(H,T), set(T).


?- set([a, b, c, c, d]).
?- set([a, b, c]).
?- set([a, [b,a], c]).
?- set([]).

-----------------------------------------------------------

% returns true if 'Atom' was found in the file 'F'

find(F, Atom) :-
    open(F, read, X),
    current_input(Stream),
    set_input(X),
    atom_codes(Atom, L),
    read_file(L, L),
    close(X),
    set_input(Stream).

read_file([], _).
    
read_file([H|L], X):-
    get_char(C),
    C \= 'end_of_file',
    atom_codes(C, CL),
    comp(H, CL),
    read_file(L, X).

read_file([H|_], X) :-
    get_char(C),
    C\='end_of_file',
    atom_codes(C, CL),
    \+comp(H, CL),
    read_file(X, X).
    
comp(H, [H|_]).



?- find('d:\\find.txt', 'one').

-----------------------------------------------------------

intersection([], _, []).
intersection([H|X], Y, [H|Z]) :- member(H, Y), !, intersection(X, Y, Z).
intersection([_|X], Y, Z) :- intersection(X, Y, Z).


% ?- intersection([2,6,12], [5,2,8,9], R).
% ?- intersection([2,[1],6], [2,[3],8,[1]], R).

-----------------------------------------------------------

% check if a string is palindrome

palindrome :-
      write("Enter a string: "),
      read(X),
      atom_codes(X,XL),
      reverse(XL,YL),
      palind(XL,YL).
      
palind([],[]):- write("is palindrome").

palind([H|X], [H|Y]) :- palind(X,Y).
palind([X|_], [Y|_]) :- X\=Y, write("not palindrome"), fail.


?- palindrome.

lol
rotor
racecar

-----------------------------------------------------------

% Program to count the words of a given file

count('end_of_file', X) :- write(X), !.
count(' ', X) :- !, NX is X+1, count(NX).
count( _ , X) :- count(X).

count(X) :- get_char(C), count(C,X).
count :-
    open('d:\\input.txt', read, Fp),
    current_input(S),
    set_input(Fp),
    count(1),
    set_input(S),
    close(Fp).
    

?- count.

-----------------------------------------------------------

largest_element(L, N) :- large(L, 0, N).

large([], N, N).
large([H|T], X, N):- X<H, large(T, H, N).
large([H|T], X, N):- X>H, large(T, X, N).

%?- largest_element([1,6,0,-1], N).

-----------------------------------------------------------

to_binary(0, []).
to_binary(X, [A|Y]) :-
     X > 0,
     A is X mod 2,
     NewX is (X-A)/2,
     to_binary(NewX, Y).
     
    
?- to_binary(5, X).

-----------------------------------------------------------

% If a number is even print 'even' else print 'odd'
even(X) :- 0 is X mod 2, write(even) ; write(odd).

?- even(4).
?- even(5).

-----------------------------------------------------------

% If number is negative print "negative number!" else print "positive number!"

negative(X) :- X<0, write("negative number!") ; write("positive number!").

?-  negative(5).
?-  negative(-5).

-----------------------------------------------------------

change(120, 121).    % Change each x (ASCII 120) to y (ASCII 121)
change(X,X) :- X\=120.
changexy(X,Y) :- atom_codes(X,XL), fetch(XL,YL), atom_codes(Y,YL).

fetch([],[]).
fetch([H|T],[X|Y]) :- change(H,X), fetch(T,Y).

%? changexy(amxhxuy, N). N=amyhyuy



