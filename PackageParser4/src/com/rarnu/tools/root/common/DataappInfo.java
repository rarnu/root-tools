package com.rarnu.tools.root.common;

import android.content.pm.ApplicationInfo;

public class DataappInfo {
    public ApplicationInfo info;
    public int type;
    public boolean checked;
    public String log;
    public String localPath;
    public int apkStatus;
    public String appName;
    public boolean installed = false;
    // 0: succ | 1: exists | 2: fail
    public int logId;
    public int position;
    public boolean installing = false;
}
