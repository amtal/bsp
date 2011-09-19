-module(bsp_tests).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

go()->
    proper:quickcheck(bsp_tests:reduction()).

reduction() ->
    ?FORALL({Tree,Limit}
           ,{?LET(Dim,integer(1,5),
                ?LET(Size,pow2_vector(Dim),
                        byte_tree(Size)
                    )
             )
            ,integer(0,255)
            }
           ,begin
        % mapping contents of the tree from a range of 0..255
        % to a range of 0..Limit should simplify the structure
        Tree2 = bsp:map(fun(N) when N>Limit->0;
                           (N) -> N
                        end, Tree),
        {N1,D} = bsp:sparsity(Tree),
        {N2,D} = bsp:sparsity(Tree2),
        N2 =< N1
    end).

pow2_vector(N) -> 
    ?LET(Range,union([1,2,4,8,16,32]), % only cubic vectors - meh, will do
        ?LET(Contents,vector(N,Range),
            list_to_tuple([{vec,N}|Contents])
        )
    ).

byte_tree(Size) ->
    FlatSize = volume(Size),
    ?LET(Data,vector(FlatSize,integer(0,255)),
        bsp:new(Size, fun(Pos)->lists:nth(index(Size,Pos)+1,Data) end)
    ).
    
% where
    volume(Vec) -> 
        [_|Vs] = tuple_to_list(Vec),
        lists:foldl(fun erlang:'*'/2, 1, Vs).
    index({_,_},{_,I}) -> I;
    index({_,A,_},{_,I,J}) -> I+J*A; % I'm sure compiler optimizes these~~
    index({_,A,B,_},{_,I,J,K}) -> I+J*A+K*A*B;
    index({_,A,B,C,_},{_,I,J,K,L}) -> I+J*A+K*A*B+L*A*B*C;
    index({_,A,B,C,D,_},{_,I,J,K,L,M}) -> I+J*A+K*A*B+L*A*B*C+M*A*B*C*D;
    index(_,_) -> error(unimplemented).
        
