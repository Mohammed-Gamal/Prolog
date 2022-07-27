d(a,b).
d(b,e).
d(b,c).
d(d,e).
d(c,d).
d(e,f).
d(g,e).

hasphone(c).

go(X, X, T).
go(X, Y, T) :- d(X,Z), \+member(Z,T), go(Z, Y, [Z|T]).
go(X, Y, T) :- d(Z,X), \+member(Z,T), go(Z, Y, [Z|T]).


?-  go(a, c, []), hasphone(c).