import os
import platform

def execmd(cmd):
    p = os.popen(cmd)
    ret = p.read()
    p.close()
    return ret


def check_os():
    o = platform.system().strip().lower()
    return o == "linux"


def check_env():
    check_ret = execmd("which mdb-export").strip()
    return check_ret == "/usr/bin/mdb-export"

