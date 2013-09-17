package com.rarnu.tools.root;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.NetworkInfo;

import com.rarnu.tools.root.common.DeviceInfo;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.common.UpdateInfo;
import com.rarnu.tools.root.utils.DeviceUtils;

public class GlobalInstance {

	public static boolean DEBUG = false;
	public static PackageManager pm = null;

	// sysapp
	public static SysappInfo currentSysapp = null;
	public static boolean allowDeleteLevel0 = false;
	public static boolean alsoDeleteData = true;
	public static boolean backupBeforeDelete = true;

	// data
	public static boolean overrideBackuped = true;
	public static boolean reinstallApk = true;
	public static String backupPath = "";

	// comp
	public static PackageInfo currentComp = null;

	// memory
	public static int myPid = 0;
	public static boolean killProcessBeforeClean = true;
	public static MemProcessInfo currentMemoryProcess = null;

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
		pm = context.getPackageManager();
		device = DeviceUtils.getDeviceInfo();
	}
}
