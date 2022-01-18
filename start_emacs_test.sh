#!/usr/bin/env sh

ARGS=
COLOR=

for arg in $*; do
    case $arg in
	    256|-nw) COLOR=256color ;;
	    88) COLOR=88color ;;
	    16) COLOR=16color ;;
	    *) ARGS="$ARGS $arg" ;;
    esac
done

[ -z "$EMACS" ] && EMACS=emacs
if [ -n "$COLOR" ]; then
    export TERM=xterm-$COLOR
    echo "Run in terminal as $TERM"
    EMACS="$EMACS -nw"
else
    echo "Run as a graphical window"
fi
$EMACS -Q --debug-init -L $(pwd) -l profile-test.el $ARGS
