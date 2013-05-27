package com.rarnu.utils;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.telephony.TelephonyManager;

/**
 * for the full feature supported, use DeviceUtils in PackageParser4 instead
 */
public class DeviceUtilsLite {

	public static String getDeviceUniqueId(Context context) {
		TelephonyManager tm = (TelephonyManager) context
				.getSystemService(Context.TELEPHONY_SERVICE);
		return tm.getDeviceId() + "_" + tm.getSubscriberId();
	}

	public static int getAppVersionCode(Context context, String filePath) {
		int versionCode = 0;
		try {
			PackageInfo pi = context.getPackageManager().getPackageArchiveInfo(
					filePath, 0);
			versionCode = pi.versionCode;
		} catch (Exception e) {

		}
		return versionCode;
	}

	public static int getAppVersionCode(Context context) {
		return getAppVersionCode(context, (ApplicationInfo) null);
	}

	public static int getAppVersionCode(Context context, ApplicationInfo info) {
		int versionCode = 0;
		String packageName = "";
		if (info != null) {
			packageName = info.packageName;
		} else {
			packageName = context.getPackageName();
		}
		try {
			PackageInfo pi = context.getPackageManager().getPackageInfo(
					packageName, 0);
			versionCode = pi.versionCode;
		} catch (Exception e) {

		}
		return versionCode;
	}

	public static String getAppVersionName(Context context) {
		return getAppVersionName(context, null);
	}

	public static String getAppVersionName(Context context, ApplicationInfo info) {
		String versionName = "";
		String packageName = "";
		if (info != null) {
			packageName = info.packageName;
		} else {
			packageName = context.getPackageName();
		}
		try {
			PackageInfo pi = context.getPackageManager().getPackageInfo(
					packageName, 0);
			versionName = pi.versionName;

		} catch (Exception e) {

		}
		return versionName;
	}
}
