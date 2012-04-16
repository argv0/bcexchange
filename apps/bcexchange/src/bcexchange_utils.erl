-module(bcexchange_utils).
-compile(export_all).

-define(RINGTOP, trunc(math:pow(2,160)-1)).  % SHA-1 space

ord_idx(Partition, Ring) ->
    Partition div (?RINGTOP div riak_core_ring:num_partitions(Ring)).
