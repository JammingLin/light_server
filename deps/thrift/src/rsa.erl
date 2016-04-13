%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2014 13:50
%%%-------------------------------------------------------------------
-module(rsa).
-author("Administrator").

%% API
-export([init/0, get_key/1, get_public_key/0, get_private_key/0, encrypt/2, decrypt/1]).

encrypt(PlainText, PubKey) ->
    public_key:encrypt_public(PlainText, PubKey).

decrypt(CipherText)->
	public_key:decrypt_private(CipherText, get_private_key()).

init() ->
	spawn(fun() ->
		ets:new(rsa_key, [public,set,named_table]),
		timer:sleep(infinity)
	end),

	{ok, PublicKey} = file:read_file("data/keys/rsa_pub_key.aes"),
    %%PublicKey = get_key("data/keys/rsa_pub.key"),

	ets:insert(rsa_key, {public_key, PublicKey}),

	PrivateKey = read_rsa_key("data/keys/rsa_pri.key"),
	ets:insert(rsa_key, {private_key, PrivateKey}).

read_rsa_key(FileName) ->
	{ok, PemBin} = file:read_file(FileName),
	[Entry] = public_key:pem_decode(PemBin),
	public_key:pem_entry_decode(Entry).

get_public_key() ->
	[{public_key, PublicKey}] = ets:lookup(rsa_key, public_key),
	PublicKey.

get_private_key() ->
	[{private_key, PrivateKey}] = ets:lookup(rsa_key, private_key),
	PrivateKey.

get_key(Filename) ->
	{ok, PemBin} = file:read_file(Filename),
	BinList = split_bin(PemBin),
	merge_binlist(BinList, "").

merge_binlist([<<"-----BEGIN PUBLIC KEY-----">> | Rest], Result) ->
	merge_binlist(Rest, Result);
merge_binlist([<<"-----END PUBLIC KEY-----">> | _Rest], Result) ->
	Result;
merge_binlist([<<"-----BEGIN PRIVATE KEY-----">> | Rest], Result) ->
	merge_binlist(Rest, Result);
merge_binlist([<<"-----END PRIVATE KEY-----">> | _Rest], Result) ->
	Result;
merge_binlist([Data | Rest], Result) ->
	merge_binlist(Rest, Result ++ binary_to_list(Data)).

split_bin(Bin) ->
	split_bin(0, Bin).

split_bin(N, Bin) ->
	case Bin of
		<<Line:N/binary, "\r\n", Rest/binary>> ->
			[Line | split_bin(0, Rest)];
		<<Line:N/binary, "\n", Rest/binary>> ->
			[Line | split_bin(0, Rest)];
		<<Line:N/binary>> ->
			[Line];
		_ ->
			split_bin(N+1, Bin)
	end.
