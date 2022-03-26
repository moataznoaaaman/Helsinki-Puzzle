/*HELPERS*/

grid_size(M,N):- 
	length(M,N1), 
	N is round(sqrt(N1)).
	

/*removes first instance of E, if present*/

remove(_,[],[]).
remove(E,[H|T],L):-
	H = E,
	L = T.
remove(E,[H|T],[H1|T1]):-
	E \= H,
	H1 = H,
	remove(E,T,T1).

/*
*gets row from grid G at index I
*decrements the index I until it reaches 1, at which the head of the grid is the row to be returned
*/
row_getter(1,[H|_],H).
row_getter(I,[H|T],R):-
    length([H|T],N),
	I=<N,
	I>1,
	I1 is I - 1,
	row_getter(I1,T,R).

/*
*gets column from grid G at index I
*from each row (which is the head), we get the Ith element, which a member of the Ith column
*/
column_getter(_,[],[]).
column_getter(I,[H|T],[HC|TC]):-
	nth1(I,H,E),
	HC = E,
	column_getter(I,T,TC).

/*compares each row with those before it to make sure they are unequal*/

compare_rows(_,_,0).
compare_rows(G,R,N):-
	row_getter(N,G,R1),
	R \= R1,
	N1 is N - 1,
	compare_rows(G,R,N1).


/*fills rows with any elements of list L*/
	
fill_row([],_).
fill_row([H|T],L):-
	member(H,L),
	fill_row(T,L).


	
nest(G,L):-
	grid_size(G,D),
	nest(G,1,D,L).
nest(G,D,D,[L]):-
	flat_row_getter(G,D,R),
	L = R.
nest(G,I,D,[H|T]):-
	flat_row_getter(G,I,R),
	H = R,
	I1 is I + 1,
	nest(G,I1,D,T).


	
flat_row_getter(G,N,R):-
	grid_size(G,S),
	I is 1 + (N-1)*S,
	flat_row_getter(G,N,R,[],I,S).

flat_row_getter(_,_,R,R,_,0).

flat_row_getter(G,N,R0,R,I,S):-
	element_getter(G,I,E),
	append(R,[E],R1),
	S1 is S - 1,
	I1 is I + 1,
	flat_row_getter(G,N,R0,R1,I1,S1).
	
element_getter([H|_],1,H).

element_getter([_|T],I,E):-
	I > 1,
	I1 is I - 1,
	element_getter(T,I1,E).


/*takes the first N elements from a list and returns it*/

take(0,_,[]).	
take(N,[H|T],[HL|TL]):-
	N \= 0,
	HL = H,
	N1 is N - 1,
	take(N1,T,TL).

/*END HELPERS*/






/*
*first, we create a list of N rows
*fill each row with N elements
*/

grid_build(N,M):-
	length(M,N),
	grid_build2(N,M).
grid_build2(_,[]).
grid_build2(N,[H|T]):-
	length(H,N),
	grid_build2(N,T).


	
/*
grid_gen2(N,[H|T]):-
	grid_build(N,[H|T]),
	num_gen(1,N,L),
	append(L,L,L1),
	fill_row(H,L1),
	grid_gen3(T,H).
grid_gen3([],_).
grid_gen3([H|T],L):-
	acceptable_permutation(L,H),
	grid_gen3(T,L).
*/

	
	
/*
*fill each row with elements from 1 to N inlcusive
*after adding each row, check if it is distinct from previous
*also make sure the first few elements of the rows corresponds to an incomplete column (row_col_match2)
*/


grid_gen(N,[H|T]):-
	grid_build(N,[H|T]),
	num_gen(1,N,L),
	fill_row(H,L),
	grid_gen(T,L,[H]).
grid_gen([],_,_).
grid_gen([H|T],L,G):-
	fill_row(H,L),
	append(G,[H],G1),
	\+member(H,G),
	row_col_match2(G1),
	grid_gen(T,L,G1).


	
/*
*F represents the first num, L represents the last
*append F to list till, incrementing F each time by 1, till F equals L
*/

num_gen(L,L,[L]).
num_gen(F,L,[F|T]):- 
	F < L,
	F1 is F + 1,
	num_gen(F1,L,T).
	
num_gen2(L,L,[L,L]).
num_gen2(F,L,[F,F|T]):-
	F < L,
	F1 is F + 1,
	num_gen2(F1,L,T).

/*all numbers from 1 to maximum, inclusive, should be a member of grid*/

check_num_grid(G1):- flat(G1,G),max_member(Max, G), min_member(Min,G), Min=1, grid_size(G,N), Max=<N, num_gen(Min,Max,L),
setof(X, member(X,G), NoDups), checkEqual(NoDups, L).

checkEqual([],[]).
checkEqual([H1|T1],[H2|T2]):- H1=H2, checkEqual(T1,T2).

flat([],[]).
flat([H|T],L):-
	flat(T,L1),
	append(H,L1,L).
	
/*
*grid is size N*N
*start with row N and column N and make sure they're unequal
*do that with every N, decrementing each time by 1, till 0
*/

acceptable_distribution(G):-
	length(G,N),
	acceptable_distribution(G,N).
	
acceptable_distribution(_,0).
acceptable_distribution(G,N):-
	row_getter(N,G,R),
	column_getter(N,G,C),
	R \= C,
	N1 is N - 1,
	acceptable_distribution(G,N1).


/*
*L represents the indices of columns the row can match with
*N represents the index of the current row
*the Nth row can match with any column at index X (X is a member of original num_gen list L), except that with the same index (hence X\= N)
*once a row matches with a column, the index of that column is deleted from L, as the other rows should match with the other columns
*continue till N is decremented to 0 and hence L will become empty (all rows must have matched with all the column indices)
*/
row_col_match(G):-
	length(G,N),
	num_gen(1,N,L),
	row_col_match(G,N,L).
row_col_match(_,0,_).
row_col_match(G,N,L):-
	row_getter(N,G,R),
	member(X,L),
	X \= N,
	column_getter(X,G,C),
	R = C,
	N1 is N - 1,
	delete(L,X,L1),
	row_col_match(G,N1,L1).


/*
*this second version is for while the grid is being generated
*S denotes the number of rows,
*the first N denotes the number of (incomplete) columns
*the other N denotes the index of row we are handling (starting from last row decrementing till first)
*note that since that columns are incomplete (not all rows are there), we take part of the row, 
equal in length to incomplete column, in order to compare them together
*/


row_col_match2([H|T]):-
	length([H|T],S),
	length(H,N),
	num_gen(1,N,L),
	row_col_match2([H|T],S,S,L).
row_col_match2(_,0,_,_).
row_col_match2(G,N,S,L):-
	row_getter(N,G,R1),
	take(S,R1,R),
	member(X,L),
	X \= N,
	column_getter(X,G,C),
	R = C,
	N1 is N - 1,
	delete(L,X,L1),
	row_col_match2(G,N1,S,L1).

	
	


	
/*
*third list (L) denotes possible numbers the head of permutated list (HP) can have
*HP should be a member of L minus the element with the same index
*remove the number chosen for HP from L
*note: while HP  cannot be the element with the same index, the following HP can be, 
and therefore a number is removed from L permanently if it is already in the permutated list
*note2: acceptable_permutation([H],[H],[H]) is used for special cases when there are duplicates in original list(1,2,2),
don't ask it may help with grid_gen
*/
acceptable_permutation(L,R):-
	acceptable_permutation(L,R,L).
acceptable_permutation([],[],_).
/*acceptable_permutation([H],[H],[H]).*/
acceptable_permutation([H|T],[HP|TP],L):-
	remove(H,L,L1),
	member(HP,L1),
	remove(HP,L,L2),
	acceptable_permutation(T,TP,L2).
	
/*
*gets the column at index N and puts it as the head
*repeat, incrementing N until it equals the dimension (S) of grid 
*/
trans(G,R):-
	length(G,S),
	trans(G,1,S,R).
trans(G,S,S,[R]):-
	column_getter(S,G,R).
trans(G,N,S,[H|T]):-
	N<S,
	column_getter(N,G,H),
	N1 is N + 1,
	trans(G,N1,S,T).
	
/*
*compares the Nth row with rows before it to make sure it is distinct from the rows above
*repeat decrementing N each time by 1 to make sure the rows above are also distinct from each other
*/
distinct_rows(G):-
	length(G,N),
	distinct_rows(G,N).
distinct_rows(_,1).
distinct_rows(G,N):-
	row_getter(N,G,R),
	N1 is N - 1,
	compare_rows(G,R,N1),
	distinct_rows(G,N1).
	
/*transpose the grid so that columns become rows and check if the rows are distinct*/
distinct_columns(G):-
	trans(G,G1),
	distinct_rows(G1).


helsinki(N,G):-
	grid_gen(N,G),
	/*row_col_match(G),*/
	distinct_columns(G),
	check_num_grid(G),
	acceptable_distribution(G).

	

