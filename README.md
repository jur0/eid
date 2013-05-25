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

There are two types of the id that can be generated:

 * binary - upper 48 bits is UNIX timestamp in milliseconds and the lower 16
bits is the sequence number
 * integer - the same as binary, but expressed as a positive integer

Usage
-----

In order to generate ids, the eid application must be started. There are two
functions for obtaining ids:

 * `eid:get_bin()` - returns a binary id
 * `eid:get_int()` - return an integer id

TODO
----

 * currently the ids are unique on one node only - add support for unique ids
within a cluster (using unique node id, such as MAC address, added to the
generated id)
