import sys

import yugioh_env
from yugioh_database import yugioh


def print_help():
    print("usage yugioh_convert_mdb.py R")
    print("usage yugioh_convert_mdb.py W <ORIDB PATH> <VERSION CODE> <OUTPUT PATH>")

if __name__ == "__main__":
    if not yugioh_env.check_os():
        print("this application must be run under Linux")
        exit()
    if not yugioh_env.check_env():
        print("you must install mdbtools via apt-get")
        exit()
    if len(sys.argv) < 2:
        print_help()
        exit()
    operation = sys.argv[1:][0]
    if operation == "R" and len(sys.argv) == 2:
        ygo = yugioh()
        ygo.get_version()
    elif operation == "W" and len(sys.argv) == 5:
        ori_db = sys.argv[2:][0]
        ver = sys.argv[3:][0]
        output = sys.argv[4:][0]
        ygo = yugioh()
        ygo.convert_mdb(ori_db, ver, output)
    else:
        print_help()