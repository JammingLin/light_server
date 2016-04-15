-module(orthogonal_list).

-on_load(init/0).

-export([create/2, get_value/3]).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
      erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
%%      PrivDir = case code:priv_dir(?MODULE) of
%%                      {error, bad_name} ->
%%                            EbinDir = filename:dirname(code:which(?MODULE)),
%%                            AppPath = filename:dirname(EbinDir),
%%                            filename:join(AppPath, "priv");
%%                      Path ->
%%                            Path
%%                end,
%%      erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).
      erlang:load_nif("../priv/orthogonal_list", 0).

create(_Rows, _Cols)->
    ?nif_stub.

get_value(_OrthogonalList, _Rows, _Cols)->
    ?nif_stub.