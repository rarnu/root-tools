package com.rarnu.tools.root.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.BuildPropInfo;

import java.util.ArrayList;
import java.util.List;

public class BuildPropUtils {

    private static final String PATH_BUILD_PROP = "/system/build.prop";

    public static List<BuildPropInfo> getBuildProp() {
        List<BuildPropInfo> list = null;
        try {
            List<String> file = FileUtils.readFile(PATH_BUILD_PROP);
            if (file != null && file.size() != 0) {
                list = new ArrayList<BuildPropInfo>();
                for (String f : file) {
                    if (f.trim().equals("") || f.trim().startsWith("#") || !f.trim().contains("=")) {
                        continue;
                    }
                    list.add(BuildPropInfo.parse(f));
                }
            }
        } catch (Exception e) {

        }

        return list;
    }

    public static boolean setBuildProp(List<BuildPropInfo> list) {
        boolean ret = false;
        String str = "";
        for (BuildPropInfo item : list) {
            str += String.format("%s=%s\n", item.buildName, item.buildValue);
        }
        try {
            FileUtils.rewriteFile(DirHelper.TEMP_DIR + "tmp.prop", str);
            CommandResult result = RootUtils.runCommand(String.format("busybox cp %stmp.prop %s", DirHelper.TEMP_DIR, PATH_BUILD_PROP), true);
            if (result != null && result.error.equals("")) {
                RootUtils.runCommand(String.format("chmod 644 %s", PATH_BUILD_PROP), true);
                ret = true;
            }
        } catch (Exception e) {

        }

        return ret;
    }
}
