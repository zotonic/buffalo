Buffalo
=======

An Erlang app for delayed process execution.

    buffalo:start().
    buffalo:queue({Module, Function, Args}, #{ timeout => Delay }).

Will call the MFA after `Delay` milliseconds. When calling queue/2
with the same MFA arguments again without the first one being executed
yet, the execution of the first one is cancelled and it will take
again `Delay` ms. for the MFA to be called.

For guaranteed execution, the MFA is called within a worker gen_server
as part of the `buffalo_worker_sup` supervisor.

Timeout
-------

The default timeout is 500 msec.

Deadline
--------

The MFA is guaranteed to be executed at a defined deadline. The default
deadline is 5 times the given timeout (defaults to 2500 msec).

A specific deadline can be specified:

    buffalo:queue({Module, Function, Args}, #{ timeout => 100, deadline => 250 }).

Drop If Running
---------------

Normally entries are dedepulicated when they are queued. With the option `is_drop_running`
the task is also not added if the task is already running.

Example:

    buffalo:queue({Module, Function, Args}, #{ is_drop_running => true }).


Unique Key
----------

Normally the tasks are deduplicated by matching on the complete MFA. It is possible
to supply an identifying key instead. This makes it possible to update the MFA associated
with the key.

    buffalo:queue(Key, MFA, #{ timeout => 50, deadline => 400 }).
