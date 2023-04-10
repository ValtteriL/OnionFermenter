#!/bin/bash

# Start tor
tor &

# Start socat to connect transparently through TOR socks proxy
socat TCP4-LISTEN:8081,fork,reuseaddr SOCKS4A:127.0.0.1:${VICTIM_ONION_ID}.onion:80,socksport=9050 &

# Start OF
/onionfermenter/bin/onionfermenter foreground &

# Wait for any process to exit
wait -n

# Exit with status of process that exited first
exit $?