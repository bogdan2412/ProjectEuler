#!/bin/bash

PROG=${1%%.ml}
corebuild -j 4 "$PROG.native" && time "./$PROG.native"
