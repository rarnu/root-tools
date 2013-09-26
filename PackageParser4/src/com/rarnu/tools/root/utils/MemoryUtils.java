package com.rarnu.tools.root.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.MemoryInfo;

public class MemoryUtils {

    public static MemoryInfo getMemoryInfo() {

        MemoryInfo ret = null;
        CommandResult result = RootUtils.runCommand("busybox free", false, null);
        if (result != null) {
            if (result.error.equals("")) {
                ret = new MemoryInfo();

                String r = result.result;
                r = r.toLowerCase();
                r = r.replace("total", "").replace("used", "").replace("free", "").replace("shared", "").replace("buffers", "").replace("mem:", "");
                r = r.replaceAll("\\s+", " ").trim();

                String[] ss = r.split(" ");
                ret.Total = Integer.parseInt(ss[0]) / 1024;
                ret.Used = Integer.parseInt(ss[1]) / 1024;
                ret.Free = Integer.parseInt(ss[2]) / 1024;
                ret.Shared = Integer.parseInt(ss[3]) / 1024;
                ret.Buffer = Integer.parseInt(ss[4]) / 1024;

            }
        }

        return ret;
    }

    public static void killProcess(int pid) {
        RootUtils.runCommand(String.format("kill %d", pid), true, null);
    }

    public static void dropCache() {
        RootUtils.runCommand("echo 3 > /proc/sys/vm/drop_caches", true, null);
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
        }
        RootUtils.runCommand("echo 0 > /proc/sys/vm/drop_caches", true, null);
    }
}
