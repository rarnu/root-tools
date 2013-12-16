#!/bin/bash

password=$1
fn=$2

sudo -S chmod 755 $fn <<EOF
$password
EOF

echo $FN

