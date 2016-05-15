q(a).
q(b).
q(c).
r(b).
r(c).
p(X) :- q(X).
p(X) :- q(X),r(X),!.