f(a)
f(b)
g(a)
g(b)
h(b)
k(X) :- f(X), g(X), h(X)
loves(vincent,mia)
loves(marcellus,mia)
jealous(A,B) :- loves(A,C), loves(B,C)
vertical(line(point(X,Y),point(X,Z)))
horizontal(line(point(X,Y),point(Z,Y)))