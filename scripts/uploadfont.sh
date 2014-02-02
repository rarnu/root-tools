#!/bin/sh

# font name
font=$1

# font path
font_path=$2;

# preview
font_preview=$3

#is top
istop=$4

# user
user=$5

# password
pass=$6

uploadfont() {
    expect -c "
        spawn scp ${font_path} ${user}@7thgen.info:~/${user}.7thgen.info/fonts/download/
        expect {
            \"password:\" {set timeout 500; send \"${pass}\r\"; }
            \"yes/no\" {set timeout 500; send \"yes\r\"; }
        }
        expect eof
    "
}

uploadpreview() {
    expect -c "
        spawn scp ${font_preview} ${user}@7thgen.info:~/${user}.7thgen.info/fonts/preview/
        expect {
            \"password:\" {set timeout 500; send \"${pass}\r\"; }
            \"yes/no\" {set timeout 500; send \"yes\r\"; }
        }
        expect eof
    "
}

uploadfont
uploadpreview
cmd="curl http://rarnu.7thgen.info/fonts/upload_font.php?name=${font}&top=${istop}"
${cmd}

echo "done"
