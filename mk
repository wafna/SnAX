#!/bin/bash

# Build Shell for Server

fail ()
{
	echo "!* $@"
	exit 1
}
announce ()
{
	echo "-- $@"
}

# mk empty on *nix
EXE=".exe"

cleanCabal ()
{
   announce clean
   cabal clean
}
buildCabal()
{
	local target="$1" ; shift
	[ -n "$target" ] || fail "Target required."
	[ -d "$target" ] || fail "Target is not a directory"
	pushd "$target"
	announce build $target
	# ensure we don't have an old build around to erroneously run
   [ -f "${EXE_PATH}" ] && (rm -v "${EXE_PATH}" || fail "Is the server still running?")
	(cabal build || fail "Build failed") 2>&1 | tee build.log
	popd
}
buildDB()
{
	buildCabal db
}
buildWS()
{
	buildCabal ws
}
startServer()
{
	local target="$1" ; shift
	[ -n "$target" ] || fail "Target required."
	[ -d "$target" ] || fail "Target is not a directory"
	announce start $target
	local EXE_PATH="${target}/dist/build/snapchat${target}/snapchat${target}${EXE}"
	[ -f "${EXE_PATH}" ] || fail "Cannot find executable: ${EXE_PATH}"
	"${EXE_PATH}" &
	local pid="$!"
	echo $pid > pid.$target
   sleep 2 # give it some time to finish spewing
   echo "pid: $pid"
}
startDB()
{
	startServer db
}
startWS()
{
	rm -rfv log/*
	startServer ws
}
stopServer()
{
	local target="$1" ; shift
	[ -n "$target" ] || fail "Target required."
	announce stop $target
	local pid=""
	# get the pid from the file or try to find it if the file's gone missing
	if [ -f pid.$target ]; then
		pid=$(cat pid.$target)
		rm -v pid.$target
	else
		echo "no pid recorded"
		# relies on the naming convention
		pid=$(ps | grep 'snapchat${target}' | awk '{print $1}')
	fi
	# kill the pid if we have one
	[ -n "$pid" ] && echo "pid: $pid" && kill $pid
	# it's usually dead by now but we might as well make sure
	wait "$pid" 2> /dev/null
}
stopDB()
{
	stopServer db
}
stopWS()
{
	stopServer ws
}

TARGET="$1" ; shift
case "$TARGET" in
   "clean" ) cleanCabal $@ ;;
   "build" ) buildCabal $@ ;;
	"start" ) startServer $@ ;;
	"stop" ) stopServer $@ ;;
	"bounce" ) 	
		stopWS
		stopDB
		buildDB
		buildWS
		startDB
		startWS
		;;
	* ) fail "Unknown target: $TARGET" ;;
esac
