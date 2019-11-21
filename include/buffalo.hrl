
%% @doc How many times longer than the specified delay we will wait.
-define(DEADLINE_MULTIPLIER, 5).

%% @doc Record used for administrating which functions are queued.
-record(buffalo_entry, {key, mfa, timer, deadline}).
