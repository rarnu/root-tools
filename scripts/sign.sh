#!/bin/sh

help() {
    echo "Usage:\r"
    echo "    sign.sh <version name> <user name> <user password> <action>\n"
    echo "action value: \r"
    echo "    0: install\n    1: uninstall old and install\n    2. upload to server\n"
    echo "example:\r"
    echo "    sign.sh 1.0.0 test_user test_password 2\n"
}

if [ $# -ne 4 ]; then
    help
    exit
fi

# version name
ver=$1

# user name
user=$2

# user password
pass=$3

# action: 
# 0:install 
# 1:uninstall and install 
# 2:upload
act=$4

signedapk=RootTools_${ver}.apk

install() {
    if [ $1 -eq 1 ]; then
        adb uninstall com.rarnu.tools.root
    fi
    adb install -r $signedapk
}

upload() {
    expect -c "
        spawn scp ${signedapk} ${user}@7thgen.info:~/${user}.7thgen.info/root_tools/download/
        expect {
            \"password:\" {set timeout 500; send \"${pass}\r\"; }
            \"yes/no\" {set timeout 500; send \"yes\r\"; }
        }
        expect eof
    "
}

rm RootTools*
cp ../out/production/RootTools/RootTools.apk ./
java -jar signapk.jar rarnu.x509.pem rarnu.pk8 RootTools.apk $signedapk

case $act in
    0)
        install 0
        ;;
    1)
        install 1
        ;;
    2)
        upload
        ;;
esac
echo "done"
