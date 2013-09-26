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
            list = new ArrayList<DiskInfo>();
            String strDi = "";
            for (int i = 1; i < dis.length; i++) {
                strDi = dis[i].replaceAll("\\s+", " ");
                DiskInfo info = DiskInfo.fromString(strDi);
                if (info != null) {
                    list.add(info);
                }
            }
        }
        return list;
    }

    public static double stringToKbytes(String str) {
        String sUnit = str.substring(str.length() - 1);
        double size = Double.parseDouble(str.substring(0, str.length() - 1));
        if (sUnit.equals("G")) {
            size = size * 1024 * 1024;
        } else if (sUnit.equals("M")) {
            size = size * 1024;
        }
        return size;
    }
}
