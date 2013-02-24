#!/usr/bin/env bash

# Since keter automatically choose binding port on behalf of server process and pass the chosen one by setting $PORT env variable, 
# we have to add this shell script layer to receive the binding port then pass to the server.
exec dist/build/em-server/em-server -p $PORT
