package com.rarnu.tools.neo.utils;


import android.content.Context;
import com.rarnu.tools.neo.api.NativeAPI;
import com.rarnu.tools.neo.data.BuildPropInfo;

import java.util.ArrayList;
import java.util.List;

public class BuildPropUtils {

    private static final String PATH_BUILD_PROP = "/system/build.prop";

    public static List<BuildPropInfo> getBuildProp() {
        List<BuildPropInfo> list = null;
        try {
            List<String> file = FileUtils.readFile(PATH_BUILD_PROP);
            if (file != null && file.size() != 0) {
                list = new ArrayList<>();
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

    public static boolean setBuildProp(Context ctx, List<BuildPropInfo> list) {
        boolean ret = false;
        String str = "";
        for (BuildPropInfo item : list) {
            str += String.format("%s=%s\n", item.buildName, item.buildValue);
        }
        try {
            NativeAPI.mount();
            ret = NativeAPI.writeFile(ctx, PATH_BUILD_PROP, str, 755);
        } catch (Exception e) {

        }
        return ret;
    }
}
