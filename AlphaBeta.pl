beginner(X):-
        X = [
            ['*','e','*','e','*'],
            ['e','R','e','R','e'],
            ['*','e','*','e','*'],
            ['e','R','e','R','e'],
            ['*','e','*','e','*']
        ].

expert(X):-
        X = [
            ['*','e','*','e','*','e','*','e','*','e','*'],
            ['e','R','e','R','e','R','e','R','e','R','e'],
            ['*','e','*','e','*','e','*','e','*','e','*'],
            ['e','R','e','R','e','R','e','R','e','R','e'],
            ['*','e','*','e','*','e','*','e','*','e','*'],
            ['e','R','e','R','e','R','e','R','e','R','e'],
            ['*','e','*','e','*','e','*','e','*','e','*'],
            ['e','R','e','R','e','R','e','R','e','R','e'],
            ['*','e','*','e','*','e','*','e','*','e','*']
        ].

/* alphabeta algorithm   general part*/
alphabetamax(State,Depth,_,_,Ret,_):-
        (isTerminal(State,_);Depth=0),!,
        utility(State,Ret),write(State), write(Ret),nl.

alphabetamin(State,Depth,_,_,Ret,_):-
        (isTerminal(State,_);Depth=0),!,
        utility(State,Ret),write(State), write(Ret),nl.

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

/*max(A,B,A,VA,_,VA):-
        A>=B,!.*/

getChildren(S,Ch):-
        setof(X,move(S,X),Ch),!.

getChildren(_,[]).


/* problem dependant part */

/* generate childrens (moves) of a state  */

opp('A','B').
opp('B','A').


/* generate children of a state  */

move(s(Turn,V),s(Nt,NV)):-
        opp(Turn,Nt),
        play2D(V,NV,V,Turn).

play([e|A],[ne|A],List,Turn, Col):-
        nth0(Col,List,e),
        write("Column "),
        write(Col),
        nl,!.
play([H|A],[H|NA],List,Turn, Col):-
        play(A,NA,List,Turn, Col).


play2D([H|Tail], NewState,State, Turn):-
        member('e',H),!,
        nth0(Row,State,H),
        write("Row "),
        write(Row),nl,
        play(H, NewH,H, Turn, Col),
        replace(State, Row, NewH, TmpS),!,
        checkRoom(TmpS, Row, Col,Turn,NewState).
play2D([H|Tail], NewTail,State, Turn):-
        play2D(Tail, NewTail,State, Turn).

replace([_|T],0,X,[X|T]).
replace([H|T],Index,X,[H|NewT]):-
    Index > -1,
    NewIndex is Index - 1,
    replace(T,NewIndex,X,NewT),!.
replace(L,_,_,L).

fillRoom(State,Row,Col,Turn,NewState):-
        (Row < 0,!;
        (   length(State, Len),
            MaxRow is Len - 3,
            Row > MaxRow,!
        );
            Col < 0,!;
        (   nth0(0,State,H) ,
            length(H, Len),
            MaxCol is Len,
            Col >= MaxCol,!
        );
       (    nth0(Row, State, Row0),
            nth0(Col, Row0, 'e'),!
       );
       (   Index1Row is Row + 1,
           nth0(Index1Row, State, Row1),
           Index1Col is Col + 1,
           nth0(Index1Col, Row1, 'e'),!
       );
       (   Index1Row is Row + 1,
           nth0(Index1Row, State, Row1),
           Index2Col is Col - 1,
           nth0(Index2Col, Row1, 'e'),!

       );
       (   Index2Row is Row + 2,
           nth0(Index2Row, State, Row2),
           nth0(Col, Row2, 'e'),!
       )),
       NewState = State.

fillRoom(State,Row,Col,Turn,NewState):-
        NewRow is Row + 1,
        nth0(NewRow, State, Roww),
        replace(Roww, Col, Turn, NRow),
        replace(State, NewRow, NRow, NewState).

checkRoom(State,Row,Col,Turn,NewState):-
         (     (   0 is Row mod 2, 1 is Col mod 2 ) ->
        (   fillRoom(State,Row,Col,Turn,TmpState),
            UpperRow is Row - 2,
            fillRoom(TmpState,UpperRow,Col,Turn,NewState))
       ;
        (   NRow is Row - 1,
            LCol is Col - 1,
            RCol is Col + 1,
            fillRoom(State,NRow,LCol,Turn,TmpState),
            fillRoom(TmpState,NRow, RCol , Turn , NewState)
        )
        ).

map(W,'A'):-
        W > 0,!.
map(W,'B'):-
        W < 0,!.
map(W,'D'):-
        W = 0,!.

notMember(_,[]).
notMember(X, [X|_]):-
    !,
    fail.
notMember(X, [_|T]):-
    notMember(X,T).

isTerminal([]).
isTerminal([H|T]):-
        notMember('e',H),
        isTerminal(T).

count([], _, 0):-!.
count([H|T], X, Result):-
        count1D(H, X, TmpResult1),
        count(T, X, TmpResult2),
        Result is TmpResult1 + TmpResult2.

count1D([X], X, 1):-!.
count1D([Y], X, 0):-
        X \= Y.
count1D([H|T], X, Result):-
        count1D([H], X, Tmp1),
        count1D(T, X, Tmp2),
        Result is Tmp1 + Tmp2.

isTerminal(s(_,S),Winner):-
        isTerminal(S),
        utility(s(_,S),W),
        map(W,Winner),!.

utility(s(_,S),W):-
        count(S,'A',Tmp1),
        count(S,'B',Tmp2),
        W is Tmp1 - Tmp2.

print2D(State):-
        print2D(State,0).
print2D([],_):-!.
print2D([H|T],RowIndex):-
        print1D(H,RowIndex),
        NewRowIndex is RowIndex + 1,
        print2D(T,NewRowIndex).
print1D([],_):-nl,!.
print1D([H|T],RowIndex):-
        (   (H = e) ->
        (
            write("  "),!
        );
        (
            (   (H = ne) ->
            (
              (0 is RowIndex mod 2 , write("__") ); (write("|"))
            );
            (write(H),write(" ")))
        )),print1D(T, RowIndex).

playerMove(State, Row, Col, NewState):-
        nth0(Row, State, RowReq),
        nth0(Col, RowReq, ColReq),
        ColReq = e,
        replace(RowReq, Col, ne, ChangedRow),
        replace(State, Row, ChangedRow, NewState),!.

run(S):-
        isTerminal(S,Win),!,
        write('Game over Winner is '),
        write(Win),nl.

run(s('A',S)):-
        alphabetamax(s('A',S),3,-30,30,_,NS),
        run(NS).%run(s(x,NS))

run(s('B',S)):-
        print2D(S),nl,write('Select the row and col number you want to play in'),
        read(Row),
        read(Col),
        playerMove(S, Row, Col, TmpNS),
        checkRoom(TmpNS,Row,Col,'B',NS),
        run(s('A',NS)).

run:-
        beginner(X),
        run(s('B',X)).




















