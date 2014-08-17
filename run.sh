#!/bin/bash

while (( "$#" )); do
    PROG=${1%%.ml}
    corebuild -j 4 -package core,core_extended "$PROG.native" &&
    time "./$PROG.native"
    shift
done
