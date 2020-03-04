#!/usr/bin/env sh

ARGS=
TERM_TEST=no
COLOR=256color

for arg in $*; do
    case $arg in
	256) COLOR=256color ;;
	88) COLOR=88color ;;
	16) COLOR=16color ;;
	*)
        [ "$arg" = '-nw' ] && TERM_TEST=yes
        ARGS="$ARGS $arg"
        ;;
    esac
done

[ "$TERM_TEST" = 'yes' ] && export TERM=xterm-$COLOR
[ -z "$EMACS" ] && EMACS=emacs
$EMACS -Q --debug-init -l test-profile.el $ARGS
