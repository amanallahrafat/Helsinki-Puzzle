%...................................................................................%
%.............................(Helsinki Puzzle).....................................%
%...................................................................................%
%1st Predicate.
grid_build(N,M):-
	length(M1,N),
	build_helper(N,M1,M).

build_helper(_,[],[]).
build_helper(N,[H|T],[H|Res]):-
	length(H,N),
	build_helper(N,T,Res).
%...................................................................................%
%2nd Predicates.
grid_gen(N,M):-
	grid_build(N,M),N1 is N-1,num_gen(0,N1,L),acceptable_permutation(L,LP),
	trans(M,MTrance),create_list(LP,M,MTrance),gen_helper(N,M),distinct_rows(M),
	check_num_grid(M).
	
gen_helper(_,[]).	
gen_helper(N,[H|T]):-
	maplist(between(1,N),H),
	gen_helper(N,T).

create_list([],_,[]).
create_list([H1|T],L,[H|R]):-
	nth0(H1,L,H),
	create_list(T,L,R).
%...................................................................................%
%3rd Predicate.
%generate a list [F....L].
num_gen(F,L,[]):-F>L.
num_gen(F,L,[H|T]):-
	F=<L,H=F,F1 is F+1,
	num_gen(F1,L,T).
%...................................................................................%
%4th Predicate.
check_num_grid(G):-
	flatten(G,FlatList),
	sort(FlatList,Sorted),
	last(Sorted,Max),length(G,N),Max=<N,num_gen(1,Max,Sorted).
%...................................................................................%
%5th predicate
acceptable_distribution(G):-
	trans(G,M1),length(G,N),N1 is N-1,check_row(G,M1,N1).
check_row(_,_,-1).
check_row(M,M2,Ind):-
	Ind>=0,check_row_colum(Ind,M,M2),Ind1 is Ind-1,check_row(M,M2,Ind1).
check_row_colum(Ind,M,M2):-
	nth0(Ind,M,R),nth0(Ind,M2,R2),R\=R2.
%...................................................................................%	
%6th predicate
%get the transpose of any matrix.
trans(Empty, []):-
	empty(Empty).
trans(M, [P|T]):- 
	first(M, P, A), trans(A, T).

empty([[]]).
empty([[]|T]):- 
	empty(T).

first([], [], []).
first([[P|A]|R], [P|Ps], [A|As]):-
	 first(R, Ps, As).
%...................................................................................%
%7th predicate
distinct_rows([]).
distinct_rows([H|T]):-
	\+ member(H,T),distinct_rows(T).
%...................................................................................%
%8th predicate
distinct_columns(M):-
	trans(M,M_Trans),distinct_rows(M_Trans).
%...................................................................................%
%9th predicate.
row_col_match(G):-
	trans(G,GT),
	acceptable_permutation(GT,G).
%...................................................................................%
%10th prediacte
acceptable_permutation(L,LP):-
	acceptable_helper(L,[],L,LP).
acceptable_helper([],_,_,[]).
acceptable_helper([H|T],Acc,L,[H1|LP]):-
	member(H1,L),H1\=H,\+ member(H1,Acc),
	acceptable_helper(T,[H1|Acc],L,LP).
%...................................................................................%
%THE GAME.	
helsinki(N,G):-
	grid_gen(N,G).
%...................................................................................%
%.............................(The End of The Game).................................%
%...................................................................................%