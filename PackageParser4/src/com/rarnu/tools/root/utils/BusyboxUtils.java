package com.rarnu.tools.root.utils;

import com.rarnu.command.RootUtils;

public class BusyboxUtils {

    public static boolean isBusyboxReady() {
        boolean a = RootUtils.hasSu();
        boolean b = RootUtils.hasSuperuser();
        boolean c = RootUtils.hasBusybox();
        return (a && b && c);
    }

}
