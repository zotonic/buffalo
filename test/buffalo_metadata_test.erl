-module(buffalo_metadata_test).

-include_lib("eunit/include/eunit.hrl").

logger_metadata_test_() ->
    {foreach,
     fun() -> application:start(buffalo) end,
     fun(_) -> application:stop(buffalo) end,
     [?_test(first_caller_metadata(Metadata, DropRunning))
      || Metadata <- [undefined, #{}, #{request_id => <<"first">>}],
         DropRunning <- [false, true]] ++
     [fun cancelled_metadata/0, fun implicit_key_metadata/0]}.

first_caller_metadata(Metadata, DropRunning) ->
    Key = make_ref(),
    FirstTask = task(Key, original),
    UpdatedTask = task(Key, updated),
    Options = #{timeout => 60000, is_drop_running => DropRunning},
    ?assertEqual({ok, new}, enqueue(Metadata, fun() ->
        buffalo:queue(Key, FirstTask, Options)
    end)),
    ?assertEqual({ok, existing}, enqueue(#{request_id => <<"second">>}, fun() ->
        buffalo:queue(Key, UpdatedTask, Options#{timeout => 0})
    end)),
    ?assertEqual({updated, Metadata}, worker_result(Key)).

cancelled_metadata() ->
    Key = make_ref(),
    Task = task(Key, cancelled),
    ?assertEqual({ok, new}, enqueue(#{request_id => <<"cancelled">>}, fun() ->
        buffalo:queue(Key, Task, #{timeout => 60000})
    end)),
    ok = buffalo:cancel(Key),
    Metadata = #{request_id => <<"new">>},
    NewTask = task(Key, new),
    ?assertEqual({ok, new}, enqueue(Metadata, fun() ->
        buffalo:queue(Key, NewTask, #{timeout => 0})
    end)),
    ?assertEqual({new, Metadata}, worker_result(Key)).

implicit_key_metadata() ->
    Ref = make_ref(),
    Task = task(Ref, implicit),
    Metadata = #{request_id => <<"first">>},
    ?assertEqual({ok, new}, enqueue(Metadata, fun() ->
        buffalo:queue(Task, #{timeout => 60000})
    end)),
    ?assertEqual({ok, existing}, enqueue(#{request_id => <<"second">>}, fun() ->
        buffalo:queue(Task, #{timeout => 0})
    end)),
    ?assertEqual({implicit, Metadata}, worker_result(Ref)).

%% Each enqueue originates in a separate process that exits before the next call.
enqueue(Metadata, Queue) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        case Metadata of
            undefined -> ok;
            _ -> logger:set_process_metadata(Metadata)
        end,
        Parent ! {Ref, Queue()}
    end),
    receive
        {'DOWN', Monitor, process, Pid, Reason} ->
            ?assertEqual(normal, Reason)
    after 1000 ->
        error(enqueue_timeout)
    end,
    receive
        {Ref, Result} -> Result
    after 1000 ->
        error(enqueue_result_timeout)
    end.

task(Ref, Tag) ->
    Parent = self(),
    {erlang, apply, [fun() ->
        Parent ! {Ref, Tag, logger:get_process_metadata()},
        ok
    end, []]}.

worker_result(Ref) ->
    receive
        {Ref, Tag, Metadata} -> {Tag, Metadata}
    after 1000 ->
        error(worker_timeout)
    end.
