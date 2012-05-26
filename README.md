Buffalo
=======

An Erlang app for delayed process execution.

    buffalo:start().
    buffalo:queue(Module, Function, Args, Delay).

Will call the MFA after `Delay` milliseconds. When calling queue/4
with the same MFA arguments again without the first one being executed
yet, the execution of the first one is cancelled and it will take
again `Delay` ms. for the MFA to be called.

For guaranteed execution, the MFA is called within a worker gen_server
as part of the `buffalo_worker_sup` supervisor.
