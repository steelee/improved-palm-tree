lives(zebra, savanna).
lives(lion, savanna).
lives(deer, forest).
lives(sloth, forest).
lives(shark, ocean).

eats(shark, meat).
eats(lion, meat).
eats(sloth, leaves).
eats(zebra, grass).
eats(deer, grass).

portal(1,blue,2).      
portal(3,red,4).      
portal(2,green,1).
portal(2,blue,3).      
portal(4,red,1).      
portal(3,green,2).
portal(3,blue,4).      
portal(4,red,2).      
portal(4,green,2).

neighbor(X,Y) :- lives(X,Z), lives(Y,Z), X \= Y.
prey(X) :- neighbor(X,Y), eats(Y, meat).

path(S,P,F) :- portal(S,P,F).
path(S,P,F) :- portal(S,P,Z), path(Z,P,F). 


