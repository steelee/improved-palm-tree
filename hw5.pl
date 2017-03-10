%   CS 381
%   Will Olsen
%   Evan Steele
%   Stefan Reindel
%   Homework 5, Prolog
%   
% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

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


%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X), parent(X,_). 
isFather(X) :- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(S,X), parent(S,Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- sibling(Y,Z), married(X,Z).
siblingInLaw(X,Y) :- sibling(X,Z), married(Y,Z).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- female(X), siblingInLaw(X,Z), parent(Z,Y).
aunt(X,Y) :- female(X), sibling(X,Z), parent(Z,Y).
uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).
uncle(X,Y) :- male(X), siblingInLaw(X,Z), parent(Z,Y).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- child(X,S), sibling(S,T), parent(T,Y).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y), ancestor(X,Z).

% Extra credit: Define the predicate `related/2`.
related(X,Z) :- related_(X,Y), related_(Y,Z).
related_(X,Y) :- ancestor(X,Y).
related_(X,Y) :- ancestor(X,Y).
related_(X,Y) :- married(X,Y).
related_(X,Y) :- married(Y,X).
related_(X,Y) :- child(Y,X).
related_(X,Y) :- child(X,Y).
related_(X,Y) :- aunt(X,Y).
related_(X,Y) :- aunt(Y,X).
related_(X,Y) :- uncle(X,Y).
related_(X,Y) :- uncle(Y,X).
related_(X,Y) :- cousin(X,Y).
related_(X,Y) :- cousin(Y,X).
related_(X,Y) :- siblingInLaw(X,Y).
related_(X,Y) :- siblingInLaw(Y,X).
related_(X,Y) :- sibling(X,Y).
related_(X,Y) :- sibling(Y,X).

%%
% Part 2. Language implementation
%%

% List notes:
% [H|T] is a list, H is the head, and T is the tail.

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.
cmd(if(_,P1), [f|T], S2) :- prog(P1,T,S2).
cmd(if(P2,_), [t|T], S2) :- prog(P2,T,S2).
cmd(lte, [H1 | [H2|T]], S2) :- X = (H1=<H2 -> Res=t;Res=f), call(X), S2 = [Res|T]. 
cmd(add, [H1 | [H2|T]], S2) :- X is H1 + H2, S2 = [X|T].
cmd(C,S1,S2) :- S2 = [C|S1].

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.

prog([], S1, S2) :- S1 = S2.
prog([H|T], S1, S2) :- cmd(H, S1, Total) , prog(T, Total, S2).

%------------------------------------











