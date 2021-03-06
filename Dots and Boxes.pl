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
alphabetamax(State,Depth,_,_,Ret,State):-
        (isTerminal(State,_);Depth=0),!,
        utility(State,Ret)/*,write(State), write(Ret),nl*/.

alphabetamin(State,Depth,_,_,Ret,State):-
        (isTerminal(State,_);Depth=0),!,
        utility(State,Ret)/*,write(State), write(Ret),nl*/.

alphabetamax(State,Depth,Alpha,Beta,Ret,Next):-
        getChildren(State,Children,Depth),
        selectChildmax(Children,Depth,Alpha,Beta,Ret,_,Next).

alphabetamin(State,Depth,Alpha,Beta,Ret,Next):-
        getChildren(State,Children,Depth),
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

getChildren(s(Turn,S),Ch,Depth):-
        bagof(X,move(s(Turn,S),X,Depth),Ch),!.

getChildren(_,[],_).


/* problem dependant part */

/* generate childrens (moves) of a state  */

opp('A','B').
opp('B','A').


/* generate children of a state  */

move(s(Turn,V),s(Nt,NV),Depth):-
        opp(Turn,Nt),
        playNoRec(V,NV,Turn,Depth).
       % write(NV),nl,nl.

playNoRec(State, NewState, Turn,Depth):-
        nth0(Row, State, _),
        nth0(Row, State, CurrRow),
        nth0(Col, CurrRow, e),
        replace(CurrRow, Col, ne, NewRow),
        replace(State, Row, NewRow, TmpS),
        checkRoom(TmpS, Row, Col,Turn,NState),
        (
               (TmpS \= NState) ->
                    (
                        (
                           (Turn = 'A') ->
                              (alphabetamax(s(Turn,NState), Depth, -25, 25, _, s(_,NewState)))
                               ;
                              (alphabetamin(s(Turn,NState), Depth, -25, 25, _, s(_,NewState)))
                        )

                    );
                    (   NewState = NState )
        ).

replace([_|T],0,X,[X|T]):-!.
replace([H|T],Index,X,[H|NewT]):-
    Index > -1,
    NewIndex is Index - 1,
    replace(T,NewIndex,X,NewT),!.
replace(L,_,_,L):-!.

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
        X \= Y,!.
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
              (0 is RowIndex mod 2 , write("__") ); (write("| "))
            );
            (write(H),write(" ")))
        )),print1D(T, RowIndex).

playerMove(State, Row, Col, NewState):-
        nth0(Row, State, RowReq),
        nth0(Col, RowReq, ColReq),
        ColReq = e,
        replace(RowReq, Col, ne, ChangedRow),
        replace(State, Row, ChangedRow, NewState),!.

difficulity(1,2).
difficulity(2,4).
difficulity(3,6).

run(s(_,S),_):-
        isTerminal(s(_,S),Win),!,
        print2D(S),
        write('Game over Winner is '),
        write(Win),nl.

run(s('A',S),Depth):-
        alphabetamax(s('A',S),Depth,-25,25,_,NS),
        run(NS,Depth).%run(s(x,NS))

run(s('B',S),Depth):-
        print2D(S),nl,
        write('p to play'),nl,
        write('s to skip'),nl,
        read(Command),
        (
         (Command = p) ->
            (
                write('Select the row and col number you want to play in '),
                read(Row),
                read(Col),
                playerMove(S, Row, Col, TmpNS),
                checkRoom(TmpNS,Row,Col,'B',NS),
                (
                  (TmpNS = NS) ->
                    (
                        run(s('A',NS),Depth)
                    );
                    (
                        run(s('B',NS),Depth)
                    )
                )

            );
            (
                run(s('A',S),Depth)
            )
        ).

run:-
        write('1.Beginner'),nl,
        write('2.Expert'),nl,
        read(Choice),
        (   (Choice = 1) ->
        (   beginner(X)
        );
        (   expert(X)
        )
        ),
        write('1.Easy'),nl,
        write('2.Medium'),nl,
        write('3.Hard'),nl,
        read(Difficulity),
        difficulity(Difficulity, Depth),
        run(s('B',X),Depth).




















