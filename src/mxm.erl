%%%-------------------------------------------------------------------
%%% @author Nikolaj Høyer         <ctl533@alumni.ku.dk>    NH
%%% @author Andrew Tristan Parli  <qcm239@alumni.ku.dk>    ATP
%%% @copyright 2016, Nikolaj Høyer and Andrew Parli
%%% @doc
%%%
%%% Description of module here
%%%
%%% @end
%%%
%%% Created: 11 Oct 2016 by ATP
%%%-------------------------------------------------------------------
-module(mxm).

-import(read_mxm, [from_file/1,
                   parse_track/1,
                   do_all/1
                  ]).

%% API
-export([total_words/1,
         mean_words/1,
         grep/2,
         reverse_index/1
        ]).


%%% -on_load(test()).  %%% run tests on load.







total_words(Data) -> true.

mean_words(Data) -> true.

grep(Word, Data) -> true.

reverse_index(Data) -> true.





