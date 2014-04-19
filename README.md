SnapChat
========

A simple web enabled messaging app demonstrating Snap, Acid-State, IxSet, Aeson, and React.

This is a small demonstration project in Haskell exploring using the Snap web framework as an HTTP API server and a combination of Acid-State and IxSet to provide acid-relational data.

Building
--------

The provided mk file should do all the work.  Most of it defers to cabal but it also does some server management.  A minor change to EXE on line 16 should get it to run on Nixes.  Alternatively you could just use cabal and run and stop the executables manually.

    mk build db
    mk build ws

Builds the whole thing.

    mk start db
    mk start ws

Starts the servers

    mk stop ws
    mk stop db

Stops the servers

    mk bounce

Stops the servers, builds the servers, then starts the servers (feeling lucky?).

Design Notes
------------

I eschewed REST here just to see what it would look like when HTTP is not part of the application stack.  In this scheme, HTTP is viewed merely as a transport layer and JSON is the transport format.  Thus, the only error codes from HTTP are the following:

* 404 - bad url.
* 400 - the message body is not parsable as JSON.
* 500 - exception from the API layer.

These reflect the situations where the client will not get an intelligible response.

Having ditched REST the API looks very much RPC: clients call functions with parameters and get data in return.  The scheme implemented here allows multiple calls to be stacked in one request for efficiency.  E.g., after logging in the set of users plus all the current user's sent and received messages are retrieved by three separate calls stacked in a single request.

Road Map
--------

* Make the DB a separate process, give it a CLI and/or a web admin page.
* Get rid of the 400 and 500 errors: bad input and api failure should be modeled in the response payload.
* Throw in Bootstrap and make the UI a little prettier.  Then, use this as a sanbox for wrapping Bootstrap in React.