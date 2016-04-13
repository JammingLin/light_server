%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 八月 2014 13:46
%%%-------------------------------------------------------------------
-module(tplt_data).
-author("long").

%% 读取模板表数据
%% API
-export([start_link/0, start_link/1]).
-export([get/1, get/2, find/2, find/3, get_all/1]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Path) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Path], []).

get(TemplateTable) when is_integer(TemplateTable) ->
    tplt_data:get(list_to_atom(integer_to_list(TemplateTable)));

get(TemplateTable) when is_atom(TemplateTable) ->
    EtsTable = single_record_templates,
    EtsKey = TemplateTable,
    case ets:lookup(EtsTable, EtsKey) of
        [] -> throw({not_found_template_data, EtsTable, EtsKey});
        [{EtsKey, Record}] -> Record
    end.

%% 根据第一列的数据值查找对应的数据
get(Table, Key)  when is_atom(Table) ->
    case ets:lookup(Table, Key) of
        [] -> throw({not_found_template_data, Table, Key});
        [{Key, Record}] -> Record
    end.

%% 根据指定的字段名查找对应对的数据
find(Table, Field, Value) when is_atom(Table), is_atom(Field) ->
    L = ets:tab2list(Table),
    [Record || {_Key, Record} <- L, Value == maps:get(Field, Record)].

%% 根据指定的字段名集合查找对应对的数据, 使用方式参看test_find的测试用例
%% FieldList: [{Field, Value}]
find(Table, FieldList) when is_atom(Table), is_list(FieldList) ->
    L = ets:tab2list(Table),
    [Record || {_Key, Record} <- L, is_record_match(FieldList, Record, true)].

is_record_match([], _Record, true) ->
	true;
is_record_match([], _Record, false) ->
	false;
is_record_match(_FieldList, _Record, false) ->
	false;
is_record_match([{Field, Value} | Rest], Record, _Result) ->
	IsMatch = Value == maps:get(Field, Record),
	is_record_match(Rest, Record, IsMatch).

%% 获得整个模板表的数据
get_all(Table) when is_atom(Table) ->
    L = ets:tab2list(Table),
    [Record || {_Key, Record} <- L].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    create_single_record_templates_table(),
    load_template_files(),
    {ok, []};
init([Path]) ->
    create_single_record_templates_table(),
	load_template_files(Path),
	{ok, []}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

create_single_record_templates_table() ->
    ets:new(single_record_templates, [public,set,named_table]).

load_template_files() ->
	load_template_files("data/template/").

load_template_files(Path) ->
    case file:list_dir(Path) of
        {ok, Filenames} ->
            [load_template_data(Path, Filename) || Filename <- Filenames];
        {error, enoent} ->
            throw({error, enoent, Path, file:get_cwd()})
    end.

load_template_data(Path, Filename) ->
    try
        {ok, Binary} = file:read_file(Path ++ Filename),
        Basename = filename:basename(Filename, ".json"),
        TemplateTable = list_to_atom(Basename),
        {ok, Content, []} = json:decode(Binary),
        case Content of
            RecordList when is_list(RecordList) ->
                save_records_to_ets(TemplateTable, RecordList);
            Record ->
                save_object_to_ets(TemplateTable, Record)
        end
    catch
        A:B  ->
            throw({A, B, Path, Filename})
    end.

save_records_to_ets(Table, RecordList) when is_list(RecordList) ->
    ets:new(Table, [public,set,named_table]),
    F = fun({obj, Fields}) ->
        PrimaryValue = get_primary_value(Fields),
        Record1 = [{list_to_atom(Key), json:parse_map_value(Value)} || {Key, Value} <-Fields],
        Map = maps:from_list(Record1),
        %% 插入时, 如果有重复的, 报错, 提醒策划修改
        case ets:insert_new(Table, {PrimaryValue, Map}) of
            true -> ok;
            false -> throw({duplicate_key, Table, PrimaryValue})
        end
    end,
    [F(Record) || Record <- RecordList],
    ok.

save_object_to_ets(TemplateName, {obj, Fields}) ->
    Table = single_record_templates,
    PrimaryValue = TemplateName,
    Record1 = [{list_to_atom(Key), json:parse_map_value(Value)} || {Key, Value} <-Fields],
    Map = maps:from_list(Record1),
    case ets:insert_new(Table, {PrimaryValue, Map}) of
        true -> ok;
        false -> throw({duplicate_key, Table, PrimaryValue})
    end,
    ok.

get_primary_value([{_, PrimaryValue} | _]) ->
    PrimaryValue.

-compile(export_all).

test_find() ->
    start_link(),
    Result = tplt_data:find(building, [{building_type, 1}, {building_level, 1}]),
	io:format("~p~n", [Result]).