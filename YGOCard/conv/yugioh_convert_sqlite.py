import sys

import yugioh_env
from yugioh_database import yugioh


def print_help():
    print("usage yugioh_convert_sqlite.py R")
    print("usage yugioh_convert_sqlite.py W <ORIDB PATH> <VERSION CODE> <OUTPUT PATH>")

if __name__ == "__main__":
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
        ygo.convert_db(ori_db, ver, output)
    else:
        print_help()