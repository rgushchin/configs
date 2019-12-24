#!/bin/bash

if ifconfig vpn0 2>&1 | grep UP > /dev/null
then
    for i in `seq 1 5`; do
	if mbsync -a
	then
	    exit 0
	fi
    done
fi
