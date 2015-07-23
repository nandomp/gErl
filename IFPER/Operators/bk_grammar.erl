-module(bk_grammar).
-compile(export_all).

%s(S) -> s(S,[]).

%s(S1,S2) -> np(S1,S3), vp(S3,S4), np(S4,S2).
%s(S1,S2) -> np(S1,S3), vp(S3,S4), np(S4,S5), prep(S5,S6), np(S6,S2).


np(S1,S2) -> det(S1) and noun(S2).
np(S1,S2,S3) -> det(S1) and adj(S2) and noun(S3).

det(a)-> true;
det(the)-> true.

adj(big)->true;
adj(small)->true;
adj(nasty)->true.

vp(S1) -> verb(S1).
vp(S1,S2) -> verb(S1) and prep(S2).

noun(man)->true;
noun(dog)->true;
noun(house)->true;
noun(ball)->true.

verb(likes)->true;
verb(walks)->true;
verb(hits)->true.

prep(at)->true;
prep(to)->true;
prep(on)->true;
prep(in)->true;
prep(into)->true.

conj('and')->true.