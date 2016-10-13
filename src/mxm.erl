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
         mean_words/0,
         grep/2,
         reverse_index/1
        ]).


%%% -on_load(test()).  %%% run tests on load.

-define(TEST_DATA, do_all("data/mxm_dataset_test.txt")).
-define(TRAIN_DATA, do_all("data/mxm_dataset_train.txt")).


% Compute the total number of words, summed over all the songs.
%    Data   : [Int]
%    Output :  Int
total_words(Data) ->

    % {_, Bins} = ?TEST_DATA,
    % {_, _, SongWordCounts} = lists:unzip3(Bins),
    % FlatData = lists:flatten(SongWordCounts),

    MapFun    = fun({_,Count}) -> Count end,
    ReduceFun = fun(Val, SumAcc) -> SumAcc + Val end,
    Initial = 0,
    CountsOnly = lists:map(MapFun, Data),
    lists:foldl(ReduceFun, Initial, CountsOnly).


%%% Computes the mean (average) number of unique words in a song
%       so really the length of the sublist in SongWordCounts

%%% Computes the mean total number of words in a song
%       so the sum of the counts for each sublist....
mean_words() ->
    {_, Bins} = ?TEST_DATA,
    {_, _, WordCountsPerSong} = lists:unzip3(Bins),
    {mean_unique_words(WordCountsPerSong), mean_total_words(WordCountsPerSong)}.


% Each sublist in WordCountsPerSong represents the WordIndex -> WordCount
% mapping for the words in the data set. Therefore, we know that each
% element in the list represents a unique word. So taking the length of a
% sublist represents the number of unique words for that song.
mean_unique_words(Data) ->
    N = length(Data),
    MapFun = fun(SubList) -> length(SubList) end,
    Values = lists:map(MapFun, Data),
    ReduceFun = fun(Value, SumAcc) -> SumAcc + Value end,
    Initial = 0,
    Sum = lists:foldl(ReduceFun, Initial, Values),
    Mean = Sum / N,
    SD = standard_deviation(Mean, Values),
    {Mean, SD}.


% Each sublist in WordCountsPerSong represents the WordIndex -> WordCount
% mapping for the words in the data set. Therefore, we can extract the
% WordCounts and sum them in order to find the total number of words for a
% song.
mean_total_words(Data) ->
    N = length(Data),
    MapFun = fun(SubList) -> total_words(SubList) end,
    ReduceFun = fun(Value, SumAcc) -> SumAcc + Value end,
    Initial = 0,
    Values = lists:map(MapFun, Data),
    Sum = lists:foldl(ReduceFun, Initial, Values),
    Mean = Sum / N,
    SD = standard_deviation(Mean, Values),
    {Mean, SD}.

%%% Computes the standard deviation of each mean
%%% sigma = sqrt( 1/N  *  sum(i=[1..N], (x_i - mean)^2  )
standard_deviation(Mean, Values) ->
    N = length(Values),
    MapFun = fun(Val) -> Diff = Val - Mean,
                         math:pow(Diff, 2)
                         end,
    Values2 = lists:map(MapFun, Values),  % Variance
    ReduceFun = fun(SD, SumAcc) -> SumAcc + SD end,
    Initial = 0,
    Sum = lists:foldl(ReduceFun, Initial, Values2),
    math:sqrt( (1/N) * Sum ).  % Sigma



%    Make a function grep that for a given word can find the mxm_track IDs for all songs with that word.
% When testing, remember that the words in the dataset are stemmed. mxm does provide one possible reverse mapping from stemmed to unstemmed words.


% ASSUMPTION FOR TESTING, IGNORE STEMMING!
grep(Word, DataSet) ->
    {WordList, Bins} = DataSet,
    % case lists:member(Word, WordList) of
    %   true  -> ok;
    %   false -> {error, Word}
    % end,
    NotWord = fun(Elem) -> Elem /= Word end,
    Index = length(lists:takewhile(NotWord, WordList)) + 1,
    MapFun = fun({Id, _, WCList}) ->
                 case lists:keymember(Index, 1, WCList) of
                     true  -> {hit, [Id]};
                     false -> {miss, none}
                 end
             end,
    ReduceFun = fun({hit, Id}, Tracks) -> lists:append(Tracks, Id);
                   ({miss, none}, Tracks) -> Tracks
                end,
    Initial = [],
    Hits = lists:map(MapFun, Bins),
    lists:foldl(ReduceFun, Initial, Hits).



% Compute a reverse index: a mapping (as a dict) from words to songs where they occur.
% DON'T TEST WITH A SINGLE PROCESS... TAKES +1.5 HOURS
reverse_index(DataSet) ->
    {WordList, Bins} = DataSet,

    MapFun = fun(Word) -> {Word, grep(Word, DataSet)} end,
    ReduceFun = fun(Tuple) -> Tuple end,

    KeyValuePairs = lists:map(MapFun, WordList),
    KeyValuePairs.


% c(mxm).
% c(read_mxm).
% {W,B} = read_mxm:do_all("data/mxm_dataset_test.txt").
% BB = lists:sublist(B, 5).
% mxm:reverse_index({W,BB}).
