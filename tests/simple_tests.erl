%%% @doc Tests ustring functions on European scripts; short strings, with
%%% composed and precombined graphemes.
%%% @author Hasan Veldstra <hasan@12monkeys.co.uk>

-module(simple_tests).

-include_lib("eunit/include/eunit.hrl").

-import(ustring, [new/1, len/1, len/2, eql/2, exact/2, upcase/1, downcase/1,
                  capitalize/1, concat/2, substr/2, substr/3, index/2, rindex/2,
                  gsub/3]).

%% Depends on List being bound in scope.
-define(ks(X),
        element(2, element(2, lists:keysearch(X, 1, List)))).

simple_test_() ->
    {setup, local,
     fun setup/0,
     fun cleanup/1,
     fun(List) ->
             [
              %% len tests
              ?_assert(len(?ks(uuml)) == len(?ks(uumlcomp))),
              ?_assert(len(?ks('Uber')) == len(?ks('Ubercomp'))),
              ?_assert(len(?ks(uuml)) == 1),
              ?_assert(len(?ks(u_uml)) == 2),
              ?_assert(len(?ks('Uber')) == 4),

              %% equality tests
              ?_assert(eql(?ks(uuml), ?ks(uumlcomp))),
              ?_assert(exact(?ks(uuml), ?ks(uumlcomp))),
              ?_assert(eql(?ks('Uber'), ?ks('Ubercomp'))),
              ?_assert(exact(?ks('Uber'), ?ks('Ubercomp'))),
              ?_assert(not eql(?ks(uuml), ?ks('Uber'))),
              ?_assert(eql(?ks('Strasse'), ?ks(strasse_up))),

              %% upcase / downcase / capitalize tests
              ?_assert(upcase(?ks(uuml)) == upcase(?ks(uumlcomp))),
              ?_assert(upcase(?ks('Uber')) == upcase(?ks('Ubercomp'))),
              ?_assert(downcase(?ks(uuml)) == downcase(?ks(uumlcomp))),
              ?_assert(downcase(?ks('Uber')) == downcase(?ks('Ubercomp'))),
              ?_assert(upcase(?ks('Strasse')) == upcase(?ks(strasse_up))),

              ?_assert(upcase(downcase(?ks('Uber'))) ==
                       upcase(downcase(?ks('Uber')))),

              ?_assert(capitalize(?ks('Strasse')) ==
                       capitalize(downcase(?ks('Strasse')))),

              %% concat tests
              ?_assert(len(concat(?ks(uuml), ?ks(uumlcomp))) ==
                       len(concat(?ks(uumlcomp), ?ks(uuml)))),
              ?_assert(eql(concat(?ks(uuml), ?ks(uumlcomp)),
                           concat(?ks(uumlcomp), ?ks(uuml)))),
              ?_assert(len(concat(?ks(uuml), ?ks(uumlcomp))) == 2),
              ?_assert(len(concat(?ks('Uber'), ?ks('Ubercomp'))) == 8),

              %% substr tests
              ?_assert(eql(substr(?ks('Uber'), 1),
                           substr(?ks('Ubercomp'), 1))),
              ?_assert(len(substr(?ks('Uber'), 2, 3)) ==
                       len(substr(?ks('Ubercomp'), 2, 3))),
              ?_assert(len(substr(?ks('Ubercomp'), 2, 3)) == 3),
              %% Concat two ü & Über, take first two characters, compare with
              %% ü concat'd with ü.
              ?_assert(eql(substr(concat(?ks(uumlcomp), ?ks('Ubercomp')), 2),
                           concat(?ks(uumlcomp), ?ks(uumlcomp)))),


              %% index/rindex tests
              %% look for precomposed ü in a string made from UTF-8 with a combined
              %% ü
              ?_assert(index(concat(?ks('Strasse'), ?ks('Ubercomp')),
                             upcase(?ks(uuml))) == 7),
              %% as above, but rindex and looking for combined ü and in precomposed
              %% string
              ?_assert(rindex(concat(?ks('Strasse'), ?ks('Uber')),
                              upcase(?ks(uumlcomp))) == 7),

              ?_assert(rindex(concat(?ks('Uber'), ?ks('Ubercomp')),
                              ?ks(uuml)) == 5),

              %% gsub tests
              %% replace "ß" with "ss"
              ?_assert(eql(gsub(?ks('Strasse'),
                                new([195, 159]),
                                new("ss")),
                           new("Strasse")) == true),

              ?_assert(eql(gsub(?ks('Ubercomp'),
                                upcase(?ks(uuml)),
                                new("U")),
                           new("Uber")) == true),
                          
              ?_assert(true == true)
             ]
     end
    }.

setup() ->
    {ok, Currd} = file:get_cwd(),
    Ebind = filename:join([Currd, "..", "ebin"]),
    code:add_patha(Ebind),
    file:set_cwd(".."),

    application:start(starling_app),

    Data =
        [
         %% U+00FC -- ü
         { uuml,       new([16#C3, 16#BC]) },
         %% U+0075 U+0308 -- u followed by combining umlaut
         { uumlcomp,   new([16#75, 16#CC, 16#88]) },
         %% U+0075 U+00A8 -- u followed by non-combining umlaut.
         { u_uml,      new([16#75, 16#C2, 16#A8]) },
         %% Über
         { 'Uber',     new([16#C3, 16#9C, 16#62, 16#65, 16#72]) },
         %% Über, but with combined Ü.
         { 'Ubercomp', new([16#55, 16#CC, 16#88, 16#62, 16#65, 16#72]) },
         %%  Straße: U+0053, U+0074, U+0072, U+0061, U+00DF, U+0065
         { 'Strasse',  new([83, 116, 114, 97, 195, 159, 101]) },
         %% STRASSE
         { strasse_up, new([83, 84, 82, 65, 83, 83, 69]) },

         { duff, <<>> }
        ],
    Data.

cleanup(_) ->
    file:set_cwd("tests"),
    application:stop(starling_app),
    ok.
