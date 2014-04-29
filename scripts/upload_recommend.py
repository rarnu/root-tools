import os
import sys
import getopt
import shutil
import pexpect
import httplib
import urllib


def print_help():
    print("usage: upload_recommend.py\n  -f <APK FILE PATH>\n  -i <ICON FILE PATH>\n  -u <UNIX NAME>\n  -m <mode 0=add, 1=update>\n  -a <USER ACCOUNT>\n  -p <USER PASSWORD>")


def do_expect_upload(file, folder, account, password):
    cmd = "scp %s %s@7thgen.info:~/%s.7thgen.info/root_tools/%s/" % (file, account, account, folder)
    print(cmd)
    spore = pexpect.spawn(cmd, timeout=None)
    for loop in range(0, 2):
        idx = spore.expect(["password", "yes/no", pexpect.EOF], timeout=None)
        if idx == 0:
            spore.send(password + "\r")
        elif idx == 1:
            spore.send("yes\r")
        elif idx == 2:
            spore.close(force=True)


def do_upload_files(apk, icon, account, password):
    do_expect_upload(apk, "package", account, password)
    do_expect_upload(icon, "icon", account, password)


def do_add(apk, icon, unix_name, package_name, label_name, account, password):
    do_upload_files(apk, icon, account, password)
    params = urllib.urlencode({"name": label_name, "package_name": package_name, "unix_name": unix_name, "icon": os.path.basename(icon), "apk": os.path.basename(apk), "mode": 0})
    conn = httplib.HTTPConnection("rarnu.7thgen.info")
    conn.request("GET", "/root_tools/admin/upload.php?" + params)
    resp = conn.getresponse()
    resp_str = resp.read()
    conn.close()
    print(resp_str)


def do_update(apk, icon, package_name, label_name, account, password):
    do_upload_files(apk, icon, account, password)
    params = urllib.urlencode({"name": label_name, "package_name": package_name, "icon": os.path.basename(icon), "apk": os.path.basename(apk), "mode": 1})
    conn = httplib.HTTPConnection("rarnu.7thgen.info")
    conn.request("GET", "/root_tools/admin/upload.php?" + params)
    resp = conn.getresponse()
    resp_str = resp.read()
    conn.close()
    print(resp_str)


def extract_package_name(str):
    str = str.replace("package: name=", "")
    str = str.replace("'", "")
    str = str[0:str.find(" ")]
    return str


def extract_label_name(str):
    str = str.replace("application-label:", "")
    str = str.replace("'", "")
    str = str.strip()
    return str


def extract_label_name_cn(str):
    str = str.replace("application-label-zh_CN:", "")
    str = str.replace("'", "")
    str = str.strip()
    return str


def do_upload(apk, icon, unix_name, mode, account, password):
    apk_info = os.popen("aapt d badging " + apk)
    lines = apk_info.readlines()
    package_name = ""
    label_name = ""
    label_name_cn = ""
    for line in lines:
        if line.__contains__("package: name=") and package_name == "":
            package_name = extract_package_name(line)
        if line.__contains__("application-label-zh_CN:") and label_name_cn == "":
            label_name_cn = extract_label_name_cn(line)
        if line.__contains__("application-label:") and label_name == "":
            label_name = extract_label_name(line)
    print("pkg:%s\nlbl_cn:%s\nlbl:%s" % (package_name, label_name_cn, label_name))
    label_name_final = label_name
    if label_name_cn != "":
        label_name_final = label_name_cn
    new_apk_file = os.path.join(os.path.dirname(apk), package_name + ".apk")
    shutil.move(apk, new_apk_file)
    apk = new_apk_file
    new_icon_file = os.path.join(os.path.dirname(icon), package_name + ".png")
    shutil.move(icon, new_icon_file)
    icon = new_icon_file
    if mode == 0:
        do_add(apk, icon, unix_name, package_name, label_name_final, account, password)
    elif mode == 1:
        do_update(apk, icon, package_name, label_name_final, account, password)


if __name__ == "__main__":
    opts, args = getopt.getopt(sys.argv[1:], "hf:i:u:m:a:p:")
    if len(opts) != 6:
        print_help()
        exit()
    apk = ""
    icon = ""
    unix_name = ""
    mode = 0
    account = ""
    password = ""
    for name, value in opts:
        if name == "-h":
            print_help()
            exit()
        elif name == "-f":
            apk = value
        elif name == "-i":
            icon = value
        elif name == "-u":
            unix_name = value
        elif name == "-m":
            mode = int(value)
        elif name == "-a":
            account = value
        elif name == "-p":
            password = value
    do_upload(apk, icon, unix_name, mode, account, password)
