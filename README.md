# eid

Erlang unique ID generator.

## ID types

An ID is composed of the UNIX timestamp in milliseconds (upper 48 bits) and a
sequence number (lower 16 bits). The sequence number is used when two UNIX
timestamp are equal. This happens when the IDs are generated quickly (more
than one ID per millisecond). The sequence number makes sure that even in this
case the IDs are unique.

There are two formats of the IDs that can be generated:

 * binary - upper 48 bits is UNIX timestamp in milliseconds and the lower 16
bits is the sequence number
 * integer - the same as binary, but expressed as a positive integer

Moreover, the generated IDs are either ascending or descending. This feature
makes it possible to sort the IDs according to when they were generated.

## Usage

In order to generate IDs, the `eid` application must be started. The API
contains the following functions:

 * `eid:get_bin()` - returns a binary ID, the generated IDs are ascending;
 * `eid:get_bin(ascend)` - the same as the previous function;
 * `eid:get_bin(descend)` - returns a binary ID, the generated IDs are
descending;
 * `eid:get_int()` - returns an integer ID, the generated IDs are ascending;
 * `eid:get_int(ascend)` - the same as the previous function;
 * `eid:get_int(descend)` - returns an integer id, the generated ids are
descending.

The functions return either `{ok, Id}` or `{error, Reason}`. The error is
returned when the sequence number is exceeded (which could theoretically
happen when there are more then 2^16 IDs generated within 1 millisecond).

*The IDs are unique just on one node!*

### Rebar dependency

The `deps` part of the `rebar.config` should include:

```erlang
  {deps, [
    ...
    {eid, {git, "https://github.com/jur0/eid.git", {branch, "master"}}},
    ...
  ]}.
```

