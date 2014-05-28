package com.rarnu.miart;

import com.rarnu.miart.command.CommandResult;
import com.rarnu.miart.command.RootUtils;

public class VMLibUtils {

    private static final String GET_VM_LIB = "getprop persist.sys.dalvik.vm.lib";
    private static final String SET_VM_LIB = "setprop persist.sys.dalvik.vm.lib lib%s.so";
    private static final String REBOOT = "reboot";
    private static final String VM_DALVIK = "dvm";
    private static final String VM_ART = "art";

    public static boolean isART() {
        CommandResult ret = RootUtils.runCommand(GET_VM_LIB, false);
        return ret != null && ret.result.contains(VM_ART);
    }

    public static boolean setArt(boolean art) {
        String cmd = String.format(SET_VM_LIB, (art ? VM_ART : VM_DALVIK));
        CommandResult ret = RootUtils.runCommand(cmd, true);
        boolean switched = false;
        if (ret != null && ret.error.equals("")) {
            // check
            switched = (art && isART()) || (!art && !isART());
            if (switched) {
                reboot();
            }
        }
        return switched;
    }

    private static void reboot() {
        RootUtils.runCommand(REBOOT, true);
    }
}
