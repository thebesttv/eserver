#!/usr/bin/env bash

emacsclient -s server -e '(kill-emacs)'
pkill -f "emacs --daemon=server"
# authbind emacs --daemon=server  # for 80 port
emacs --daemon=server
emacsclient -nw -s server -e '(load "~/eserver/main-server.el")' -n
