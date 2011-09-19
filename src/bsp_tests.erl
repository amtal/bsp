-module(bsp_tests).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

go()->proper:module(bsp_tests).

%% Via bsp:new/2, bsp:at/2, assert:
%%  Indexes are in the right place.
prop_indexes() ->
    ?FORALL(Size, ?LET(Dim,integer(1,5),pow2_vector(Dim,4)),
        ?FORALL(Data, binary(volume(Size)),
            ?FORALL(Tree, byte_tree(Size,Data), 
                ?FORALL(Point, rand_pos_vec(Size),
                    bsp:at(Point,Tree) == binary:at(Data,index(Size,Point)))))).


%% Via bsp:new/2, bsp:map/1, and bsp:sparsity/1, assert:
%%  Reducing the range of the contents tends to simplify the tree.
prop_compression() ->
    ?FORALL({Tree,Limit}
           ,{?LET(Dim,integer(1,3),
                ?LET(Size,pow2_vector(Dim,5),
                    ?LET(Data,binary(volume(Size)),
                        byte_tree(Size,Data))))
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
        Compression = 100-trunc(100*N2/N1),
        collect( with_title("Large limit = small range reduction = low compression\n"
                            "Small limit = large range reduction = high compression\n"
                            "FRQ% {Limit div 10, Compression%}:")
               , {Limit div 10, Compression}
               , N2 =< N1
               )
    end).

pow2_vector(Dims,Limit) -> 
    ?LET(Length,frequency([{X,X}||X<-lists:seq(0,Limit)]),begin
        Contents = lists:duplicate(Dims,trunc(math:pow(2,Length))),
        list_to_tuple([{vec,Dims}|Contents])
    end).

rand_pos_vec(CubeV) -> 
    [Tag|Es] = tuple_to_list(CubeV),
    Fst = hd(Es),
    true = lists:all(fun(X)->X==Fst end, Es),
    ?LET(Es2, vector(length(Es), integer(0,Fst)), list_to_tuple([Tag|Es2])).

byte_tree(Size,Data) ->
    bsp:new(Size, fun(Pos)->binary:at(Data,index(Size,Pos)) end).
    
volume(Vec) -> 
    [_|Vs] = tuple_to_list(Vec),
    lists:foldl(fun erlang:'*'/2, 1, Vs).
index({_,_},{_,I}) -> I;
index({_,A,_},{_,I,J}) -> I+J*A; % I'm sure compiler optimizes these~~
index({_,A,B,_},{_,I,J,K}) -> I+J*A+K*A*B;
index({_,A,B,C,_},{_,I,J,K,L}) -> I+J*A+K*A*B+L*A*B*C;
index({_,A,B,C,D,_},{_,I,J,K,L,M}) -> I+J*A+K*A*B+L*A*B*C+M*A*B*C*D;
index(_Size,_Point) -> error(unimplemented).
    
