from os import getenv as osenv
from shutil import copyfile as shcopy
from os.path import exists as pathexists
from platform import uname as uname

class JetBrainsConfig:

    __home__ = ""

    __base__ = ["/config", "/Library/Preferences"]
    __apps__ = None
    __apps_linux__ = ["/.IntelliJIdea14", "/.PyCharm40", "/.WebStorm9", "/.WebIde80"]
    __apps_mac__ = ["/IntelliJIdea14", "/PyCharm40", "/WebStorm9", "/WebIde80"]

    __paths__ = ["/colors/", "/codestyles/", "/keymaps/"]
    __files__ = ["rarnu_color.icls", "rarnu_codestyle.xml", "rarnu_keymap.xml"]

    __supported = True

    def __init__(self):
        self.__home__ = osenv("HOME")
        __os__ = uname()[0].lower()
        self.__supported = (__os__ == "linux" or __os__ == "darwin")
        if __os__ == "linux":
            self.__apps__ = self.__apps_linux__
        else:
            self.__apps__ = self.__apps_mac__

    def __copy_file(self):

        for base in self.__base__:
            for os_path in self.__apps__:
                __path = self.__home__ + base + os_path

                if pathexists(__path):
                    for i in range(0, 3):
                        file_path = __path + self.__paths__[i] + self.__files__[i]
                        shcopy(self.__files__[i], file_path)
                        print(file_path)

    def execute(self):
        if not self.__supported:
            print("os not supported.")
        else:
            self.__copy_file()
            print("done")
        pass

if __name__ == "__main__":
    cfg = JetBrainsConfig()
    cfg.execute()
