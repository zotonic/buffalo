-module(buffalo_worker_test).

-include_lib("eunit/include/eunit.hrl").

logger_metadata_test_() ->
    [?_test(check_metadata(Metadata))
     || Metadata <- [undefined, #{}, #{request_id => <<"worker-test">>}]].

check_metadata(Metadata) ->
    Original = logger:get_process_metadata(),
    try
        set_metadata(Metadata),
        Parent = self(),
        Ref = make_ref(),
        Task = fun() ->
            Parent ! {Ref, logger:get_process_metadata()},
            ok
        end,
        {ok, _Pid} = buffalo_worker:start_link({erlang, apply, [Task, []]}),
        receive
            {Ref, WorkerMetadata} ->
                ?assertEqual(Metadata, WorkerMetadata)
        after 1000 ->
            error(worker_timeout)
        end,
        ?assertEqual(Metadata, logger:get_process_metadata())
    after
        set_metadata(Original)
    end.

set_metadata(undefined) ->
    logger:unset_process_metadata();
set_metadata(Metadata) ->
    logger:set_process_metadata(Metadata).
