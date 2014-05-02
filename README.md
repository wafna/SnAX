SnAX
========

A simple web enabled messaging app demonstrating Snap, Acid-State, IxSet, Aeson, and React.

This is a small demonstration project in Haskell exploring using the Snap web framework as an HTTP API server and a combination of Acid-State and IxSet to provide acid-relational data.

Building
--------

The provided mk file should do all the work.  Most of it defers to cabal but it also does some server management.  A minor change to EXE on line 16 should get it to run on Nixes.  Alternatively you could just use cabal and run and stop the executables manually.

    mk build db ws

Builds the whole thing.

    mk start db ws

Starts the servers

    mk stop ws db

Stops the servers

    mk bounce

Stops the servers, builds the servers, then starts the servers (feeling lucky?).

Design Notes
------------

I eschewed REST here just to see what it would look like when HTTP is not part of the application stack.  In this scheme, HTTP is viewed merely as a transport layer and JSON is the transport format.  Thus, the only HTTP error codes would be generated by the Snap framework (e.g. 404). These reflect situations where the request never got to the API layer.

Having ditched REST the API looks very much RPC: clients call functions with parameters and get data in return.  The scheme implemented here allows multiple calls to be stacked in one request for efficiency.  E.g., after logging in the set of users plus all the current user's sent and received messages are retrieved by three separate calls stacked in a single request.  Problems with the entire request (e.g. unparsability) or with an individual request (e.g. wrong parameter types) are modeled a la Either.  After that, each function defines its own return types.

Road Map
--------

* Make a CLI for the DB.
* Load tests.
* Ponder some convenient scheme for relational consistency.
* See if Common could be made lighter by moving as much as possible to DB.

VMs
---------

* hashicorp/precise32
$ sudo apt-get update
$ sudo apt-get install haskell-platform