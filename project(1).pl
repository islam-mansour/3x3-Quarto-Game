/* alphabeta algorithm   general part*/
alphabetamax(State,Depth,_,_,Ret,_):-
        (isTerminal(State,_);Depth=70),!,
        getHeuristic(State,Ret),write(State), write(Ret),nl.

alphabetamin(State,Depth,_,_,Ret,_):-
        (isTerminal(State,_);Depth=70),!,
        getHeuristic(State,Ret),write(State), write(Ret),nl.

alphabetamax(State,Depth,Alpha,Beta,Ret,Next):-
        getChildren(State,Children),
        selectChildmax(Children,Depth,Alpha,Beta,Ret,_,Next).

alphabetamin(State,Depth,Alpha,Beta,Ret,Next):-

        getChildren(State,Children),
        selectChildmin(Children,Depth,Alpha,Beta,Ret,_,Next).

	
min(A,B,A,VA,_,VA):-
		B>=A,!.
min(_,B,B,_,VB,VB).

max(A,B,A,VA,_,VA):-
		A>=B,!.
max(_,B,B,_,VB,VB).


selectChildmax(_,_,A,B,A,_,_):-
        B=<A,!.
selectChildmin(_,_,A,B,B,_,_):-
        B=<A,!.

/*traverse children list to get best*/

selectChildmax([],_,Alpha,_,Alpha,BestTillNow,BestTillNow).

selectChildmin([],_,_,Beta,Beta,BestTillNow,BestTillNow).

selectChildmax([H|T],Depth,Alpha,Beta,Ret,BestTillNow,SelectChild):-
        NDepth is Depth -1,
        alphabetamin(H,NDepth,Alpha,Beta,NRet,_),
        max(Alpha,NRet,UpdetedAlpha,BestTillNow,H,NewBest),
        selectChildmax(T,Depth,UpdetedAlpha,Beta,Ret,NewBest,SelectChild).
        
selectChildmin([H|T],Depth,Alpha,Beta,Ret,BestTillNow,SelectChild):-
        NDepth is Depth -1,
        alphabetamax(H,NDepth,Alpha,Beta,NRet,_),
       
        min(Beta,NRet,UpdetedBeta,BestTillNow,H,NewBest),
        selectChildmin(T,Depth,Alpha,UpdetedBeta,Ret,NewBest,SelectChild).
		
		
getChildren(S,Ch):-
        bagof(X,move(S,X),Ch),!.
getChildren(_,[]).

        
		
/* problem dependant part */

/* generate childrens (moves) of a state  */

opp(x,o).
opp(o,x).


/* generate children of a state  */

move(s(X,V),s(Y,NV)):-
        opp(X,Y),
        play(V,NV,[b,r,t]).

move(s(X,V),s(Y,NV)):-
        opp(X,Y),
        play(V,NV,[w,r,t]).

move(s(X,V),s(Y,NV)):-
        opp(X,Y),
        play(V,NV,[w,r,s]).

move(s(X,V),s(Y,NV)):-
        opp(X,Y),
        play(V,NV,[b,s,t]).


move(s(X,V),s(Y,NV)):-
        opp(X,Y),
        play(V,NV,[w,s,t]).

move(s(X,V),s(Y,NV)):-
        opp(X,Y),
        play(V,NV,[b,s,s]).

move(s(X,V),s(Y,NV)):-
        opp(X,Y),
        play(V,NV,[w,s,s]).
		
move(s(X,V),s(Y,NV)):-
        opp(X,Y),
        play(V,NV,[b,r,s]).


play([[e,e,e]|A],[T|A],T).
play([H|A],[H|NA],T):-
        play(A,NA,T).

count([],0).
count([_|T],R):-
        count(T,CT),
        R is CT + 1.

del(_,[],[]).

del(X,[X|T],T):-!.

del(X,[H|T],[H|NT]):-
        del(X,T,NT).

member(X,[X|_]).
member(X,[_|T]):-
        member(X,T).

map(x,1).
map(o,-1).
map(d,0).
/* get utility function of a state  if it is terminal*/
getHeuristic(S,H):-
        isTerminal(S,W),!,
        map(W,H).
		
/* get utility function of a state */
getHeuristic(s(_,V),H):-
        countWin(V,[1,2,3],[1,2,3],[4],[0],1,1,x,CO),
        countWin(V,[1,2,3],[1,2,3],[4],[0],1,1,o,CX),
        H is  CO-CX .
/*  count all possible when of a state if i want to count wins of x 
    then T term should be o to check against o  and vice versa 

    assuming that n wins = 8 (max possible wins )untill i found opponnent letter 
	then decrease number of wins by 1 through remove element from R list or C list or D1 list or D2 list
 using Row list and Column  list and diagonal1 list and diagonal2 list */

countWin([],R,C,D1,D2,_,_,_,Ret):-
        count(R,CR),
        count(C,CC),
        count(D1,D1C),
        count(D2,D2C),
        Ret is CR + CC + D1C + D2C.
        
countWin(V,R,C,D1,D2,I,4,T,Ret):-!,
        NI is I + 1,
        countWin(V,R,C,D1,D2,NI,1,T,Ret).
        
countWin([T|V],R,C,D1,D2,I,J,T,Ret):-!,
        del(I,R,NR),
        del(J,C,NC),
        A is I+J,
        B is I-J,
        del(A,D1,ND1),
        del(B,D2,ND2),
        NJ is J+1,
        countWin(V,NR,NC,ND1,ND2,I,NJ,T,Ret).

countWin([_|V],R,C,D1,D2,I,J,T,Ret):-
        NJ is J+1,
        countWin(V,R,C,D1,D2,I,NJ,T,Ret).

/* check if  a state is terminal */

isTerminal(s(R, [
					[A,_,_], [A,_,_], [A,_,_], 
					[_,_,_], [_,_,_], [_,_,_], 
					[_,_,_], [_,_,_], [_,_,_]
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [
					[_,A,_], [_,A,_], [_,A,_], 
					[_,_,_], [_,_,_], [_,_,_], 
					[_,_,_], [_,_,_], [_,_,_]
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [
					[_,_,A], [_,_,A], [_,_,A], 
					[_,_,_], [_,_,_], [_,_,_], 
					[_,_,_], [_,_,_], [_,_,_]
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [
					[_,_,_], [_,_,_], [_,_,_], 
					[A,_,_], [A,_,_], [A,_,_], 
					[_,_,_], [_,_,_], [_,_,_]
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,_,_], 
					[_,A,_], [_,A,_], [_,A,_], 
					[_,_,_], [_,_,_], [_,_,_]
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,_,_],	
					[_,_,A], [_,_,A], [_,_,A], 
					[_,_,_], [_,_,_], [_,_,_]
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,_,_],	
					[_,_,_], [_,_,_], [_,_,_], 
					[A,_,_], [A,_,_], [A,_,_]
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,_,_], 
					[_,_,_], [_,_,_], [_,_,_],
					[_,A,_], [_,A,_], [_,A,_]				
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,_,_],
					[_,_,_], [_,_,_], [_,_,_],
					[_,_,A], [_,_,A], [_,_,A]				
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[A,_,_], [_,_,_], [_,_,_],
					[_,_,_], [A,_,_], [_,_,_],
					[_,_,_], [_,_,_], [A,_,_]					
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,A,_], [_,_,_], [_,_,_],
					[_,_,_], [_,A,_], [_,_,_],
					[_,_,_], [_,_,_], [_,A,_]					
				]),R):-
		write(R),nl,
        \+A=e.
isTerminal(s(R, [ 
					[_,_,A], [_,_,_], [_,_,_],
					[_,_,_], [_,_,A], [_,_,_],
					[_,_,_], [_,_,_], [_,_,A]					
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [
					[_,_,_], [_,_,_], [A,_,_],
					[_,_,_], [A,_,_], [_,_,_],
					[A,_,_], [_,_,_], [_,_,_]
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,A,_],
					[_,_,_], [_,A,_], [_,_,_],
					[_,A,_], [_,_,_], [_,_,_]					
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,_,A],
					[_,_,_], [_,_,A], [_,_,_],
					[_,_,A], [_,_,_], [_,_,_]			
				]),R):-
		write(R),nl,	
        \+A=e.
		
isTerminal(s(R, [ 
					[A,_,_], [_,_,_], [_,_,_],
					[A,_,_], [_,_,_], [_,_,_],
					[A,_,_], [_,_,_], [_,_,_]			
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,A,_], [_,_,_], [_,_,_],
					[_,A,_], [_,_,_], [_,_,_],
					[_,A,_], [_,_,_], [_,_,_]			
				]),R):-
		write(R),nl,	
        \+A=e.

isTerminal(s(R, [ 
					[_,_,A], [_,_,_], [_,_,_],
					[_,_,A], [_,_,_], [_,_,_],
					[_,_,A], [_,_,_], [_,_,_]			
				]),R):-
		write(R),nl,
        \+A=e.

isTerminal(s(R, [ 
					[_,_,_], [A,_,_], [_,_,_],
					[_,_,_], [A,_,_], [_,_,_],
					[_,_,_], [A,_,_], [_,_,_]			
				]),R):-
		write(R),nl,	
        \+A=e.

isTerminal(s(R, [ 
					[_,_,_], [_,A,_], [_,_,_],
					[_,_,_], [_,A,_], [_,_,_],
					[_,_,_], [_,A,_], [_,_,_]			
				]),R):-
		write(R),nl,	
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,A], [_,_,_],
					[_,_,_], [_,_,A], [_,_,_],
					[_,_,_], [_,_,A], [_,_,_]			
				]),R):-
		write(R),nl,	
        \+A=e.

		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [A,_,_],
					[_,_,_], [_,_,_], [A,_,_],
					[_,_,_], [_,_,_], [A,_,_]			
				]),R):-
		write(R),nl,
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,A,_],
					[_,_,_], [_,_,_], [_,A,_],
					[_,_,_], [_,_,_], [_,A,_]			
				]),R):-
		write(R),nl,	
        \+A=e.
		
isTerminal(s(R, [ 
					[_,_,_], [_,_,_], [_,_,A],
					[_,_,_], [_,_,_], [_,_,A],
					[_,_,_], [_,_,_], [_,_,A]			
				]),R):-
		write(R),nl,	
        \+A=e.


isTerminal(s(_,L),d):-
        \+member([e,e,e],L).

        

print([],_,_):-
		nl,write('---------------------------'),nl,nl.

print([H|T],N,A):-
        (N=0,nl,write('---------------------------'),nl;true),
        NN is (N+1) mod 3,
        NA is A + 1,
        write(H),
        print(T,NN,NA).

output(L):-
        print(L,0,1).


playerMove([[e,e,e]|T],1,[X|T],X):-!.

playerMove([H|T],N,[H|NT],X):-
        NN is N-1,
        playerMove(T,NN,NT,X).
		
getRes(o, 'you win').		
getRes(x, 'machine win').		
getRes(d, 'draw').

run(S):-
		write(here),nl,nl,
        isTerminal(S, Win),!,
		write(here),nl,nl,
		getRes(Win, T),
		write('Game is Over, Result: '),
		write(T), nl.

run(s(o,S)):-
        alphabetamax(s(o,S),10,-30,30,_,NS), /*********************/
		run(NS).
	
run(s(x,S)):-
        output(S),nl,
		write('Select the cell number: '),
        read(C),
		write('Select the the piece ex.(wrt -> white round tall): '),
        read(H),
		atom_chars(H,L2),
        playerMove(S,C,NS,L2),
        run(s(o,NS)).

run:-
        run(s(x,[
					[e,e,e], [e,e,e], [e,e,e],
					[e,e,e], [e,e,e], [e,e,e],
					[e,e,e], [e,e,e], [e,e,e]
				])).
