package com.rarnu.huawei.p6.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;

public class CpuUtils {

    public static String getIntValue(String name) {
        CommandResult result = RootUtils.runCommand(String.format("cat %s", name), true);
        String ret = "";
        if (result.error.equals("")) {
            ret = result.result;
        }
        return ret;
    }

    public static boolean setIntValue(String name, String value) {
        CommandResult result = RootUtils.runCommand(String.format("echo %s > %s", value, name), true);
        return result.error.equals("");
    }

    public static int getStringIndex(String[] arr, String str) {
        int idx = -1;
        for (int i = 0; i < arr.length; i++) {
            if (arr[i].equals(str)) {
                idx = i;
                break;
            }
        }
        if (idx == -1 && str.equals("0")) {
            idx = 0;
        }
        return idx;
    }

    public static String getIndexedString(String[] arr, int index) {
        String ret = "0";
        if (index != 0) {
            ret = arr[index];
        }
        return ret;
    }


}
