#!/bin/sh

ACCOUNT=$1
PASSWORD=$2
FILE=$3
FOLDER=$4

expect -c "

spawn scp ${FILE} ${ACCOUNT}@7thgen.info:~/${ACCOUNT}.7thgen.info/root_tools/${FOLDER}/
expect {
    \"password:\" { set timeout 500; send \"${PASSWORD}\r\"; }
    \"yes/no\": { set timeout 500; send \"yes\r\"; }
}

expect eof
"
