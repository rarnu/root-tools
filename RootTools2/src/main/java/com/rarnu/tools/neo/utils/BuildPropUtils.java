package com.rarnu.tools.neo.utils;


import android.os.Environment;
import com.rarnu.tools.neo.data.BuildPropInfo;
import com.rarnu.tools.neo.root.CommandResult;
import com.rarnu.tools.neo.root.RootUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class BuildPropUtils {

    private static final String PATH_BUILD_PROP = "/system/build.prop";
    private static final String PATH_BUILD_PROP_TMP = PATH_BUILD_PROP + ".tmp";

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

    public static boolean setBuildProp(List<BuildPropInfo> list) {
        boolean ret = false;
        String str = "";
        for (BuildPropInfo item : list) {
            str += String.format("%s=%s\n", item.buildName, item.buildValue);
        }
        String tmpDir = Environment.getExternalStorageDirectory().getAbsolutePath();
        File fDir = new File(tmpDir, ".tmp");
        if (!fDir.exists()) {
            fDir.mkdirs();
        }
        String tmpFile = new File(fDir, "tmp.prop").getAbsolutePath();
        try {
            FileUtils.rewriteFile(tmpFile, str);
            RootUtils.mountRW();
            CommandResult result = RootUtils.runCommand(new String[]{
                    String.format("cp %s %s", tmpFile, PATH_BUILD_PROP_TMP),
                    String.format("chmod 755 %s", PATH_BUILD_PROP_TMP),
                    String.format("cp %s %s", PATH_BUILD_PROP_TMP, PATH_BUILD_PROP)
            }, true);
            return result.error.equals("");
        } catch (Exception e) {

        }
        return ret;
    }
}
