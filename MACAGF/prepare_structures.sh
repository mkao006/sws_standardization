#!/bin/sh

echo "Run this from ~macro/REFMACRO/bin/lnx/MACAGF"
echo "Have you modified the input files in the IN sub-directory?"
echo "Have you modified the LAST_YEAR env variable in ~macro/environ.ksh?"
read -p "y to continue, n to abort $:"
if [[ "$REPLY" != "y" ]]
then
	exit -1
fi

set +x

rundefextr

run psg

run_ags
run_ags CBD
run_ags CBS
run_ags MGO

echo
echo "OK - you should be good to go now"
echo "Please check then remove any \".out\" files from this directory"
