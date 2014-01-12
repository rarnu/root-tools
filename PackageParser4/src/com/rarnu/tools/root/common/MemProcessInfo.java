package com.rarnu.tools.root.common;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import com.rarnu.tools.root.GlobalInstance;

public class MemProcessInfo {

    public String USER;
    public int PID;
    public int PPID;
    public int VSIZE;
    public int RSS;
    public String WCHAN;
    public String PC;
    public String STAT;
    public String NAME;
    public ApplicationInfo appInfo = null;
    public int position;

    public static MemProcessInfo stringToProcessInfo(Context context, String str) throws Exception {
        str = str.replaceAll("\\s+", " ");
        String[] ss = str.split(" ");
        MemProcessInfo info = new MemProcessInfo();
        info.USER = ss[0];
        info.PID = Integer.parseInt(ss[1]);
        info.PPID = Integer.parseInt(ss[2]);
        info.VSIZE = Integer.parseInt(ss[3]) / 1024;
        info.RSS = Integer.parseInt(ss[4]) / 1024;
        info.WCHAN = ss[5];
        info.PC = ss[6];
        info.STAT = ss[7];
        info.NAME = ss[8];

        try {
            info.appInfo = findApplicationByNamespace(context, ss[8]);
        } catch (NameNotFoundException e) {
        }

        if (info.NAME.equals("com.rarnu.tools.root")) {
            GlobalInstance.myPid = info.PID;
        }

        return info;
    }

    private static ApplicationInfo findApplicationByNamespace(Context context, String ns) throws NameNotFoundException {
        ApplicationInfo info = null;
        if (context != null) {
            info = context.getPackageManager().getApplicationInfo(ns, 0);
        }
        return info;
    }
}
