package com.rarnu.tools.root;

import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.NetworkInfo;
import android.util.DisplayMetrics;

import com.rarnu.tools.root.api.UpdateInfo;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.common.SysappInfo;

public class GlobalInstance {

	public static boolean DEBUG = false;
	public static float density = 0;
	public static DisplayMetrics metric = new DisplayMetrics();
	public static PackageManager pm = null;

	// network
	public static boolean loadingNetwork = false;
	public static NetworkInfo networkInfo = null;
	public static String networkSpeed = "";

	// sysapp
	public static SysappInfo currentSysapp = null;
	public static boolean allowDeleteLevel0 = false;
	public static boolean alsoDeleteData = true;
	public static boolean backupBeforeDelete = true;

	// data
	public static boolean overrideBackuped = true;
	public static boolean reinstallApk = true;

	// comp
	public static PackageInfo currentComp = null;

	// memory
	public static int myPid = 0;
	public static boolean killProcessBeforeClean = true;
	public static MemProcessInfo currentMemoryProcess = null;

	// host
	public static String nameServer = "8.8.8.8";

	// update
	public static UpdateInfo updateInfo = null;
	public static boolean isFirstStart = true;

	// device info
	public static String deviceId = "";
	public static String module = "";
	public static String osVersion = "";
	public static String mail = "";
	public static String buildDescription = "";
}
