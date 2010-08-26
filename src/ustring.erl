%%% @doc Ustring - Unicode strings for Erlang.
%%% @author Hasan Veldstra <hasan@12monkeys.co.uk> [http://12monkeys.co.uk/starling]

-module(ustring).

%% Strings <-> Ustrings
-export([new/1, new/2, empty/0, to/2, pr/1]).

%% Information.
-export([len/1, len/2]).

%% String transformation.
-export([upcase/1, downcase/1, capitalize/1, capitalize/2]).

%% String processing.
-export([concat/2, substr/2, substr/3, index/2, rindex/2, gsub/3]).

%% Comparison.
-export([eql/2, exact/2]).

%% Standard string module compatibility layer.
-export([to_upper/1, to_lower/1, equal/2, str/2, rstr/2]).

-export([timeout/0]).

-define(is_binary2(Bin1, Bin2), is_binary(Bin1) andalso is_binary(Bin2)).
-define(is_binary3(Bin1, Bin2, Bin3),
        is_binary(Bin1) andalso is_binary(Bin2) andalso is_binary(Bin3)).

%% @doc Creates a ustring from a list or binary of UTF-8 code units.
new(List) when is_list(List) ->
    new(utf8, List);
new(Bin) when is_binary(Bin) ->
    new(utf8, Bin).

new(utf8, List) when is_list(List) ->
    Uc = xmerl_ucs:from_utf8(List),
    Utf16Be = xmerl_ucs:to_utf16be(Uc),
    {ok, Norm} = call({normalize, unpack(list_to_binary(Utf16Be))++[256]}),
    pack(Norm);
new(utf8, Bin) when is_binary(Bin) ->
    new(utf8, binary_to_list(Bin)).

%% @doc Creates an empty string.
empty() ->
    new([]).

%% @doc Converts string to a list of UTF-8 code units.
to(utf8, Str) when is_binary(Str) ->
    Uc = xmerl_ucs:from_utf16be(binary_to_list(Str)),
    xmerl_ucs:to_utf8(Uc).

%%% @doc Makes string printable.
pr(Str) when is_binary(Str) ->
    to(utf8, Str).

%% @doc Returns length of string in graphemes (characters as users see them).
len(Str) when is_binary(Str) ->
    {ok, Len} = call({lengthg, unpack(Str)++[256]}),
    Len.

%% @doc Returns length of string in code units (2-byte pairs).
len(Str, codeunits) when is_binary(Str) ->
    {ok, Len} = call({length, unpack(Str)++[256]}),
    Len.

%% @doc Converts string to uppercase.
upcase(Str) when is_binary(Str) ->
    {ok, Res} = call({upcase, unpack(Str)++[256]}),
    pack(Res).

%% @doc Converts string to lowercase.
downcase(Str) when is_binary(Str) ->
    {ok, Res} = call({downcase, unpack(Str)++[256]}),
    pack(Res).

%% @doc Capitalizes the first word in the string, leaves the rest unchanged.
capitalize(Str) when is_binary(Str) ->
    {ok, Res} = call({capitalize, unpack(Str)++[256]}),
    pack(Res).
%% @doc Capitalizes every word in the string.
capitalize(Str, words) when is_binary(Str) ->
    {ok, Res} = call({capitalize_words, unpack(Str)++[256]}),
    pack(Res).

%% @doc Compares strings (case-sensitive).
exact(Str1, Str2) when ?is_binary2(Str1, Str2) ->
    {ok, Bool} = call({exact, unpack(Str1)++[256], unpack(Str2)++[256]}),
    Bool.

%% @doc Compares strings (case-insensitive).
eql(Str1, Str2) when ?is_binary2(Str1, Str2) ->
    {ok, Bool} = call({equal, unpack(Str1)++[256], unpack(Str2)++[256]}),
    Bool.

%% @doc Concatenates two strings.
concat(Str1, Str2) when ?is_binary2(Str1, Str2) ->
    {ok, Res} = call({concat, unpack(Str1)++[256], unpack(Str2)++[256]}),
    pack(Res).

%% @doc Get substring of length Len beginning at the first character.
substr(Str, Len) when is_binary(Str) ->
    substr(Str, 1, Len).
%% @doc Get substring of length Len beginning at character Start.
substr(Str, Start, Len) when is_binary(Str) ->
    {ok, Res} = call({substr, unpack(Str)++[256], {Start, Len}}),
    pack(Res).

%% @doc Returns the position of first occurence of Substr in Str.
index(Str, Substr) when ?is_binary2(Str, Substr) ->
    {ok, Idx} = call({index, unpack(Str)++[256], unpack(Substr)++[256]}),
    Idx.

%% @doc Returns the position of last occurence of Substr in Str.
rindex(Str, Substr) when ?is_binary2(Str, Substr) ->
    {ok, Idx} = call({rindex, unpack(Str)++[256], unpack(Substr)++[256]}),
    Idx.

%% @doc Returns a string in which all occurences of Old have been replaced
%% with New.
gsub(Str, Old, New) when ?is_binary3(Str, Old, New) ->
    {ok, NewStr} = call({gsub, unpack(Str)++[256], unpack(Old)++[256],
                         unpack(New)++[256]}),
    pack(NewStr).
    

%%% Standard string module compatibility API.

%% @equiv upcase/1
to_upper(Str) ->
    upcase(Str).

%% @equiv downcase/1
to_lower(Str) ->
    downcase(Str).

%% @equiv exact/2
equal(Str1, Str2) ->
    exact(Str1, Str2).

%% @equiv index/2
str(Str, Substr) ->
    index(Str, Substr).

%% @equiv rindex/2
rstr(Str, Substr) ->
    rindex(Str, Substr).

%%% Utility functions.

call(Msg) ->
    {ok, Group} = application:get_env(starling, group),
    Server = pg2:get_closest_pid(Group),
    gen_server:call(Server, Msg, timeout()).

timeout() ->
    {ok, Value} = application:get_env(starling, timeout),
    Value.

%%% ----------------- %%%
%%% Packing functions %%%
%%% ----------------- %%%

%% Takes a list of bytes and makes an int out of each byte pair, e.g.
%% <<1, 1>> => [257]. Byte pairs are assumed to be big-endian.
%% Values larger than (2^16 - 1) will overflow.
from_bytep(Bin) when is_binary(Bin) ->
    from_bytep(Bin, []).
from_bytep(<<Msb:8, Lsb:8, Rest/binary>>, Acc) ->
    from_bytep(Rest, [(Msb bsl 8) bor Lsb | Acc]);
from_bytep(<<>>, Acc) ->
    lists:reverse(Acc).
    
%% Reverse of from_bytep, e.g.
%% [257, 511] => <<1, 1, 1, 255>>
to_bytep(List) when is_list(List) ->
    to_bytep(List, <<>>).
to_bytep([X | Rest], AccBin) ->
    Msb = X bsr 8,
    Lsb = X band 2#11111111,
    to_bytep(Rest, <<AccBin/binary, Msb:8, Lsb:8>>);
to_bytep([], AccBin) ->
    AccBin.
    
unpack(Ustr) when is_binary(Ustr) ->
    from_bytep(Ustr).

pack(List) when is_list(List) ->
    to_bytep(List).
