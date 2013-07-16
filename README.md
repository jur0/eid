eid
===

Erlang unique id generator.

Id types
--------

The id is composed of the UNIX time stamp in milliseconds (upper 48 bits) and a
sequence number (lower 16 bits). The sequence number is used in case there are
more than one equal time stamps. This happens when ids are generated quickly, so
they have the same time stamp. The sequence number is there to make these ids
unique.

There are two formats of the ids that can be generated:

 * binary - upper 48 bits is UNIX timestamp in milliseconds and the lower 16
bits is the sequence number
 * integer - the same as binary, but expressed as a positive integer

Moreover, the generated ids are either ascending or descending. This feature
makes it possible to sort the ids according to when they were generated.

Usage
-----

In order to generate ids, the eid application must be started. The API contains
the following functions:

 * `eid:get_bin()` - returns a binary id, the generated ids are ascending
 * `eid:get_bin(ascend)` - the same as the previous function
 * `eid:get_bin(descend)` - returns a binary id, the generated ids are descending
 * `eid:get_int()` - returns an integer id, the generated ids are ascending
 * `eid:get_int(ascend)` - the same as the previous function
 * `eid:get_int(descend)` - returns an integer id, the generated ids are
descending

The funtions return either `{ok, Id}` or `{error, Reason}`. The error is
returned when the sequence number is exceeded (which could teoretically happen
when there are more then 2^16 ids generated within 1 microsecond).

The ids are unique just on one node!
