package com.rarnu.tools.root.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;

import java.util.ArrayList;
import java.util.List;

public class BusyboxUtils {

    private static final String[] requiredApplets = new String[]{
            "cp", "du", "find", "xargs", "chmod", "grep", "reboot", "free", "unzip",
    };

    public static boolean isBusyboxReady() {
        boolean a = RootUtils.hasSu();
        boolean b = RootUtils.hasSuperuser();
        boolean c = RootUtils.hasBusybox();
        return (a && b && c);
    }

    public static boolean isAppletReady() {
        boolean ret = false;
        if (RootUtils.hasBusybox()) {
            CommandResult result = RootUtils.runCommand("busybox --list", false);
            if (result != null && result.error.equals("")) {
                String[] applets = result.result.split("\n");
                if (applets != null && applets.length != 0) {
                    ret = checkRequiredApplets(applets);
                }
            }

        }
        return ret;
    }

    private static boolean checkRequiredApplets(String[] list) {
        int count = 0;
        List<String> applets = new ArrayList<String>();
        for (String s : list) {
            applets.add(s);
        }
        for (String s : requiredApplets) {
            if (applets.indexOf(s) != -1) {
                count++;
            }
        }
        return (count == requiredApplets.length);
    }

}
