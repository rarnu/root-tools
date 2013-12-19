package com.rarnu.tools.root.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.DiskInfo;

import java.util.ArrayList;
import java.util.List;

public class DiskUtils {

    public static List<DiskInfo> getDiskInfoList() {
        CommandResult cmdRet = RootUtils.runCommand("df", false);
        String[] dis = cmdRet.result.split("\n");
        List<DiskInfo> list = null;
        if (dis != null && dis.length != 0) {
            // skip the first line
            dis[0] = "";
            list = new ArrayList<DiskInfo>();
            String strDi = "";
            for (String d : dis) {
                if (!d.equals("")) {
                    strDi = d.replaceAll("\\s+", " ");
                    DiskInfo info = DiskInfo.fromString(strDi);
                    if (info != null) {
                        list.add(info);
                    }
                }
            }
        }
        return list;
    }

    public static double stringToKbytes(String str) {
        double size = 0D;
        try {
            String sUnit = str.substring(str.length() - 1);
            size = Double.parseDouble(str.substring(0, str.length() - 1));
            if (sUnit.equals("G")) {
                size = size * 1024 * 1024;
            } else if (sUnit.equals("M")) {
                size = size * 1024;
            }
        } catch (Exception e) {

        }
        return size;
    }
}
