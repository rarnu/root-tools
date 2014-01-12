package com.rarnu.tools.root.utils;

import android.content.Context;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.MemProcessInfo;

import java.util.ArrayList;
import java.util.List;

public class ProcessUtils {

    public static List<MemProcessInfo> getUserProcessList(Context context) {
        return getProcessList(context);
    }

    private static List<MemProcessInfo> getProcessList(Context context) {

        List<MemProcessInfo> ret = null;

        CommandResult result = RootUtils.runCommand("toolbox ps", false, null);
        int position = 0;
        if (result != null) {
            if (result.error.equals("")) {
                String r = result.result;
                r = r.toLowerCase();
                // r = r.replaceAll("\\s+", " ");
                String[] ss = r.split("\n");
                ret = new ArrayList<MemProcessInfo>();
                for (String si : ss) {
                    if (!si.startsWith("root")) {
                        MemProcessInfo info = null;
                        try {
                            info = MemProcessInfo.stringToProcessInfo(context, si);
                        } catch (Exception e) {
                            info = null;
                        }

                        if (info == null) {
                            continue;
                        }
                        if (info.PID > 127) {
                            info.position = position;
                            ret.add(info);
                            position++;
                        }
                    }
                }
            }
        }
        return ret;
    }
}
