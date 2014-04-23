import os
import os.path
import shutil

PATH_IDEA = "/.IntelliJIdea13"
PATH_PYCHARM = "/.PyCharm30"
PATH_WEBSTORM = "/.WebStorm8"
PATH_PHPSTORM = "/.WebIde70"

PATH_COLOR = "/config/colors/"
PATH_CODESTYLE = "/config/codestyles/"
PATH_KEYMAP = "/config/keymaps/"

FILE_COLOR = "rarnu_color.icls"
FILE_CODESTYLE = "rarnu_codestyle.xml"
FILE_KEYMAP = "rarnu_keymap.xml"


def copy_file(path):
    shutil.copyfile(FILE_COLOR, path + PATH_COLOR + FILE_COLOR)
    shutil.copyfile(FILE_CODESTYLE, path + PATH_CODESTYLE + FILE_CODESTYLE)
    shutil.copyfile(FILE_KEYMAP, path + PATH_KEYMAP + FILE_KEYMAP)
    pass


if __name__ == "__main__":
    home = os.getenv("HOME")
    copy_file(home + PATH_IDEA)
    copy_file(home + PATH_PYCHARM)
    copy_file(home + PATH_WEBSTORM)
    copy_file(home + PATH_PHPSTORM)
    print("done")
