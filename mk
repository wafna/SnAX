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

APP_NAME=snapchat
APP_PATH="dist/build/${APP_NAME}/${APP_NAME}"
EXE_PATH="${APP_PATH}.exe"

TARGET="$1" ; shift

cleanCabal ()
{
   announce clean
   cabal clean
}
buildCabal()
{
	announce build
	# ensure we don't have an old build around to erroneously run
   [ -f "${EXE_PATH}" ] && (rm -v "${EXE_PATH}" || fail "Is the server still running?")
	(cabal build || fail "Build failed") 2>&1 | tee build.log
}
startServer()
{
	announce start
   rm -rfv log/*
	[ -f "${EXE_PATH}" ] || fail "Cannot find executable: ${EXE_PATH}"
	"${EXE_PATH}" &
	local pid="$!"
	echo $pid > server.pid
   sleep 2 # give it some time to finish spewing
   echo "pid: $pid"
}
stopServer()
{
	announce stop
	local pid=""
	# get the pid from the file or try to find it if the file's gone missing
	if [ -f server.pid ]; then
		pid=$(cat server.pid)
		rm -v server.pid
		ls -F
	else
		echo "no pid recorded"
		pid=$(ps | grep '${APP_PATH}' | awk '{print $1}')
	fi
	# kill the pid if we have one
	[ -n "$pid" ] && echo "pid: $pid" && kill $pid
	# it's usually dead by now but we might as well make sure
	wait "$pid" 2> /dev/null
	ps | grep "${APP_PATH}"
}
bounce()
{
	stopServer
	buildCabal $@
	startServer 
}

case "$TARGET" in
	"" ) bounce $@ ;;
   "clean" ) cleanCabal $@ ;;
   "build" ) buildCabal $@ ;;
	"start" ) startServer ;;
	"stop" ) stopServer ;;
	"bounce" ) bounce $@ ;;
	* ) fail "Unknown target: $TARGET" ;;
esac
