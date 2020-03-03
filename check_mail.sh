#!/bin/bash

for i in `seq 1 5`; do
    if mbsync -a
    then
	exit 0
    fi
done
