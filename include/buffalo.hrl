
%% @doc Default timeout - 500 msec
-define(DEFAULT_TIMEOUT, 500).

%% @doc How many times longer than the specified delay we will wait.
-define(DEADLINE_MULTIPLIER, 5).

%% @doc Record used for administrating which functions are queued.
-record(buffalo_entry, {
    key :: buffalo:key(),
    mfa :: buffalo:mfargs(),
    timer :: reference(),
    deadline :: non_neg_integer(),
    logger_metadata = undefined :: logger:metadata() | undefined
}).
