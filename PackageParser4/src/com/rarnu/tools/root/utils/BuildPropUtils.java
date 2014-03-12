package com.rarnu.tools.root.utils;

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

    public static void setBuildProp(List<BuildPropInfo> list) {
        // TODO: writeback tyo build.prop

    }
}
