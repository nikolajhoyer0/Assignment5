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






%%% Compute the total number of words, summed over all the songs.
total_words(Data) ->
    % extract the data needed
    {Words, Bins} = do_all(),
    {Track_id, MXM_id, WordIndexCount} = Bins,
    {Index, Count} = lists:unzip(WordIndexCount),
    SumWords = fun(Elem, Acc) -> Elem + Acc end,

    % map-reduce
    lists:foldl(SumList, 0, Count).




%%% Computes the mean (average) number of unique words in a song
%%% Computes the mean total number of words in a song
%%% Computes the standard deviation of each mean
%%%
%%% sigma = sqrt( 1/N  *  sum(i=[1..N], (x_i - mean)^2  )
%%%
mean_words(Data) -> true.





grep(Word, Data) -> true.

reverse_index(Data) -> true.





