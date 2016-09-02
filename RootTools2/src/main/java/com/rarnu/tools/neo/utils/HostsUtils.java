package com.rarnu.tools.neo.utils;

import com.rarnu.tools.neo.root.CommandResult;
import com.rarnu.tools.neo.root.RootUtils;

public class HostsUtils {

    public static final String STANDARD_HOSTS = "127.0.0.1  localhost\n255.255.255.255	broadcasthost\n::1	localhost\nfe80::1%lo0	localhost\n";
    public static final String NOUPDATE_HOSTS = STANDARD_HOSTS + "\n127.0.0.1 update.miui.com";

    public static boolean restoreHosts() {
        RootUtils.mountRW();
        String cmd = "echo '%s' > /system/etc/hosts";
        CommandResult result = RootUtils.runCommand(String.format(cmd, STANDARD_HOSTS), true);
        return result.error.equals("");
    }

    public static boolean writeNoUpdate() {
        RootUtils.mountRW();
        String cmd = "echo '%s' > /system/etc/hosts";
        CommandResult result = RootUtils.runCommand(String.format(cmd, NOUPDATE_HOSTS), true);
        return result.error.equals("");
    }

}
