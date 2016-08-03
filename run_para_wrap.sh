#! /bin/bash

PN=$1; shift 1

./$PN $*
retc=$?
killall $PN
exit $retc
