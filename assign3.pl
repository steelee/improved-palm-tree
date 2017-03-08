female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).

%------------------------------------

child(X,Y) :- parent(Y,X).

isMother(X) :- female(X), parent(X,_). 
isFather(X) :- male(X), parent(X,_).

grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

sibling(X,Y) :- parent(S,X), parent(S,Y), X \= Y.

brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).

siblingInLaw(X,Y) :- sibling(Y,Z), married(X,Z).
siblingInLaw(X,Y) :- sibling(X,Z), married(Y,Z).

aunt(X,Y) :- female(X), siblingInLaw(X,Z), parent(Z,Y).
aunt(X,Y) :- female(X), sibling(X,Z), parent(Z,Y).
uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).
uncle(X,Y) :- male(X), siblingInLaw(X,Z), parent(Z,Y).

cousin(X,Y) :- child(X,S), sibling(S,T), parent(T,Y).

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y), ancestor(X,Z).

%level(X,Y) :- level((siblingInLaw(sibling(X,S),T), X \= T),Y).
