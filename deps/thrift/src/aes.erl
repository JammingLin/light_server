%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十二月 2014 14:57
%%%-------------------------------------------------------------------
-module(aes).
-author("Administrator").

%% API
-export([encrypt/3, decrypt/3, hex_to_binary/1]).

encrypt(IOList, Key, IV) ->
    Bin = iolist_to_binary(IOList),
    Data = crypto:block_encrypt(aes_cbc128, Key, IV, pad_pkcs7(Bin)),
    %% TODO: 下面的长度检测代码， 当觉得加密算法稳定后， 可以去除
    0 = byte_size(Data) rem 16,
    Data.

decrypt(Bytes, Key, Vector) ->
    %% TODO: 下面的长度检测代码， 当觉得加密算法稳定后， 可以去除
    0 = byte_size(Bytes) rem 16,
    Bin = crypto:block_decrypt(aes_cbc128, Key, Vector, Bytes),
    unpad_pkcs7(Bin).

hex_to_binary(String) ->
    hex_to_binary(String, 1, []).
hex_to_binary(String, Start, Result) when Start >= length(String)->
    Result1 = lists:reverse(Result),
    iolist_to_binary(Result1);
hex_to_binary(String, Start, Result) ->
    Hex = string:substr(String, Start, 2),
    Int = list_to_integer(Hex, 16),
    hex_to_binary(String, Start+2, [<<Int>> | Result]).

pad_pkcs7(Bin) ->
    Diff = byte_size(Bin) rem 16,
    pad(Bin, 16-Diff).

pad(Bin, 1) ->
    <<Bin/binary,1>>;
pad(Bin, 2) ->
    <<Bin/binary,2,2>>;
pad(Bin, 3) ->
    <<Bin/binary,3,3,3>>;
pad(Bin, 4) ->
    <<Bin/binary,4,4,4,4>>;
pad(Bin, 5) ->
    <<Bin/binary,5,5,5,5,5>>;
pad(Bin, 6) ->
    <<Bin/binary,6,6,6,6,6,6>>;
pad(Bin, 7) ->
    <<Bin/binary,7,7,7,7,7,7,7>>;
pad(Bin, 8) ->
    <<Bin/binary,8,8,8,8,8,8,8,8>>;
pad(Bin, 9) ->
    <<Bin/binary,9,9,9,9,9,9,9,9,9>>;
pad(Bin, 10) ->
    <<Bin/binary,10,10,10,10,10,10,10,10,10,10>>;
pad(Bin, 11) ->
    <<Bin/binary,11,11,11,11,11,11,11,11,11,11,11>>;
pad(Bin, 12) ->
    <<Bin/binary,12,12,12,12,12,12,12,12,12,12,12,12>>;
pad(Bin, 13) ->
    <<Bin/binary,13,13,13,13,13,13,13,13,13,13,13,13,13>>;
pad(Bin, 14) ->
    <<Bin/binary,14,14,14,14,14,14,14,14,14,14,14,14,14,14>>;
pad(Bin, 15) ->
    <<Bin/binary,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15>>;
pad(Bin, 16) ->
    <<Bin/binary,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16>>.

unpad_pkcs7(Bin) ->
    Length = byte_size(Bin),
    Padding = binary:last(Bin),
    DataLength = Length-Padding,
    <<Data:DataLength/binary, _:Padding/binary>> = Bin,
    Data.