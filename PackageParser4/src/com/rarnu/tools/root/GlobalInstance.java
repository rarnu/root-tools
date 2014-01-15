package com.rarnu.tools.root;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.NetworkInfo;
import com.rarnu.tools.root.common.*;
import com.rarnu.tools.root.utils.DeviceUtils;

import java.util.ArrayList;
import java.util.List;

public class GlobalInstance {

    public static boolean DEBUG = false;
    public static boolean isOfficialVersion = true;

    // global
    public static boolean showFloatWindow = false;
    public static int floatWindowPosX = 0;
    public static int floatWindowPosY = 0;
    public static boolean theme = false;

    // sysapp
    public static SysappInfo currentSysapp = null;
    public static boolean allowDeleteLevel0 = false;
    public static boolean alsoDeleteData = true;
    public static boolean backupBeforeDelete = true;
    // data
    public static boolean overrideBackuped = true;
    public static boolean reinstallApk = true;
    public static String backupPath = "";
    public static String batchInstallPath = "";
    // comp
    public static PackageInfo currentComp = null;
    // memory
    public static int myPid = 0;
    public static boolean killProcessBeforeClean = true;
    public static MemProcessInfo currentMemoryProcess = null;
    public static List<FileOperationInfo> listOperation = new ArrayList<FileOperationInfo>();
    // network
    public static boolean loadingNetwork = false;
    public static NetworkInfo networkInfo = null;
    public static String networkSpeed = "";
    // host
    public static String nameServer = "8.8.8.8";
    // update
    public static UpdateInfo updateInfo = null;
    public static boolean isFirstStart = true;
    public static DeviceInfo device = null;

    public static void init(Context context) {
        device = DeviceUtils.getDeviceInfo();
        listOperation.clear();
    }
}
