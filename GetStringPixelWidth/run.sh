#!/bin/bash

STR=$1

ip=`./gspw -fn ubuntu -fs 20 -s "$STR" -e w`
echo "width:"$ip



