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
-export([total_words/0,
         mean_words/0%,
         % grep/0,
         % reverse_index/0
        ]).


%%% -on_load(test()).  %%% run tests on load.

-define(TEST_DATA, do_all("data/mxm_dataset_test.txt")).
-define(TRAIN_DATA, do_all("data/mxm_dataset_train.txt")).

total_words() ->
    % Compute the total number of words, summed over all the songs.

    {_, Bins} = ?TEST_DATA,
    {_, _, SongWordCounts} = lists:unzip3(Bins),
    FlatData = lists:flatten(SongWordCounts),

    MapFun = fun({_,Count}) -> Count end,
    ReduceFun = fun(N, Sum) -> Sum + N end,
    Acc0 = 0,

    CountsOnly = lists:map(MapFun, FlatData),
    lists:foldl(ReduceFun, Acc0, CountsOnly).


%%% Computes the mean (average) number of unique words in a song
%       so really the length of the sublist in SongWordCounts

%%% Computes the mean total number of words in a song
%       so the sum of the counts for each sublist....

%%% Computes the standard deviation of each mean
%%%
%%% sigma = sqrt( 1/N  *  sum(i=[1..N], (x_i - mean)^2  )

mean_words() ->
    {_, Bins} = ?TEST_DATA,
    {_, _, WordCountsPerSong} = lists:unzip3(Bins),

    MapFun1    = fun(SubList) -> length(SubList) end,
    ReduceFun1 = fun(N, Sum) -> Sum + N end,
    NumSongs = length(WordCountsPerSong),

    UniqueWordsPerSong = lists:map(MapFun1, WordCountsPerSong),
    UniqueWords = lists:foldl(ReduceFun1, 0, UniqueWordsPerSong),

    Avg_UniqueWords = UniqueWords / NumSongs.

    MapFun2 = fun(SubList) ->



% grep(Word, Data) -> true.




% reverse_index(Data) -> true.