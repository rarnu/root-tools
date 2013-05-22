#!/bin/bash

STR=$1

ip=`./gspw -fn ubuntu -fs 20 -s "$STR" -e w`
w=$(((1280 - $ip) / 2))
echo "width:"$w




