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

EXE=""
[ "$(uname)" == "MINGW32_NT*" ] && EXE=".exe"

cleanCabal ()
{
	local target="$1" ; shift
	[ -n "$target" ] || fail "Target required."
	[ -d "$target" ] || fail "Target is not a directory: $target"
	pushd "$target"
   announce clean $target
   cabal clean
	popd
}
buildCabal()
{
	local target="$1" ; shift
	[ -n "$target" ] || fail "Target required."
	[ -d "$target" ] || fail "Target is not a directory: $target"
	pushd "$target"
	announce build $target
	# ensure we don't have an old build around to erroneously run
   [ -f "${EXE_PATH}" ] && (rm -v "${EXE_PATH}" || fail "Is the server still running?")
	(cabal build || fail "Build failed") 2>&1 | tee build.log
	popd
}
startServer()
{
	local target="$1" ; shift
	[ -n "$target" ] || fail "Target required."
	[ -d "$target" ] || fail "Target is not a directory: $target"
	announce start $target
	local EXE_PATH="${target}/dist/build/snax${target}/snax${target}${EXE}"
	[ -f "${EXE_PATH}" ] || fail "Cannot find executable: ${EXE_PATH}"
	"${EXE_PATH}" &
	local pid="$!"
	echo $pid > pid.$target
   sleep 2 # give it some time to finish spewing
   echo "pid: $pid"
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
		pid=$(ps | grep 'snax${target}' | awk '{print $1}')
	fi
	# kill the pid if we have one
	[ -n "$pid" ] && echo "pid: $pid" && kill $pid
	# it's usually dead by now but we might as well make sure
	wait "$pid" 2> /dev/null
}
optAction()
{
	echo $@
	local action="$1" ; shift
	[ -n "$action" ] || fail "Action required."
	[ -n "$1" ] || fail "No target(s) supplied."
	while [ -n "$1" ]
	do
		local target="$1" ; shift
		"$action" "$target"
	done
}

TARGET="$1" ; shift
case "$TARGET" in
   "clean" ) optAction cleanCabal $@ ;;
   "build" ) optAction buildCabal $@ ;;
	"start" ) optAction startServer $@ ;;
	"stop" ) optAction stopServer $@ ;;
	"bounce" )
		optAction stopServer ws db
		optAction buildCabal db ws
		optAction startServer db ws
		;;
	* ) fail "Unknown target: $TARGET" ;;
esac
