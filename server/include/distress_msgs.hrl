%
% Client Messages.
%
-record(add_file,{block_count}).
-record(add_block,{key,value}).
-record(del_block,{key,oid}).
-record(get_block,{key}).

%
% Server Messages.
%
-record(add_ack,{oid}).
-record(block,{key,value}).
-record(error,{value}).
-record(success,{value}).
