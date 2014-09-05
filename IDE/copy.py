from os import getenv as osenv
from shutil import copyfile as shcopy
from os.path import exists as pathexists
from platform import uname as uname

class JetBrainsConfig:

    __home = ""
    __path_idea = ""
    __path_pycharm = ""
    __path_webstorm = ""
    __path_phpstorm = ""

    __path_color = "/config/colors/"
    __path_codestyle = "/config/codestyles/"
    __path_keymap = "/config/keymaps/"

    __file_color = "rarnu_color.icls"
    __file_codestyle = "rarnu_codestyle.xml"
    __file_keymap = "rarnu_keymap.xml"

    __supported = True

    def __init__(self):
        self.__home = osenv("HOME")
        os_name = uname()[0].lower()
        if os_name.lower() == "linux":
            self.__path_idea = "/.IntelliJIdea13"
            self.__path_pycharm = "/.PyCharm30"
            self.__path_webstorm = "/.WebStorm8"
            self.__path_phpstorm = "/.WebIde70"
            self.__supported = True
        elif os_name.lower() == "darwin":
            path_pref = "/Library/Preferences"
            self.__path_idea = path_pref + "/IntelliJIdea13"
            self.__path_pycharm = path_pref + "/PyCharm30"
            self.__path_webstorm = path_pref + "/WebStorm8"
            self.__path_phpstorm = path_pref + "/WebIde70"
            self.__supported = True
        else:
            self.__supported = False

    def __copy_file(self, path):
        color_path = path + self.__path_color
        codestyle_path = path + self.__path_codestyle
        keymap_path = path + self.__path_keymap
        if pathexists(color_path):
            shcopy(self.__file_color, color_path + self.__file_color)
        if pathexists(codestyle_path):
            shcopy(self.__file_codestyle, codestyle_path + self.__file_codestyle)
        if pathexists(keymap_path):
            shcopy(self.__file_keymap, keymap_path + self.__file_keymap)

    def execute(self):
        if not self.__supported:
            print("os not supported.")
        else:
            self.__copy_file(self.__home + self.__path_idea)
            self.__copy_file(self.__home + self.__path_idea)
            self.__copy_file(self.__home + self.__path_idea)
            self.__copy_file(self.__home + self.__path_idea)
            print("done")
        pass

if __name__ == "__main__":
    cfg = JetBrainsConfig()
    cfg.execute()
