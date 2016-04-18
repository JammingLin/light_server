-module(orthogonal_list_nif).

-on_load(init/0).

-export([create/2, release/1, get_value/3, update/4, print_list/1, foreach/2]).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    erlang:load_nif("../priv/orthogonal_list", 0).

create(_Rows, _Cols)->
    ?nif_stub.

release(_OrthogonalListPtr)->
    ?nif_stub.

get_value(_OrthogonalListPtr, _Rows, _Cols)->
    ?nif_stub.

update(_OrthogonalListPtr, _Rows, _Cols, _Value)->
    ?nif_stub.

print_list(_OrthogonalListObj)->
    ?nif_stub.

foreach(_OrthogonalListObj, _Callback)->
    ?nif_stub.