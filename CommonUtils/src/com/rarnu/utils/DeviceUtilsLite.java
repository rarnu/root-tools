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
		String uid = "";
		if (context != null) {
			try {
				TelephonyManager tm = (TelephonyManager) context
						.getSystemService(Context.TELEPHONY_SERVICE);
				String did = tm.getDeviceId();
				String sid = tm.getSubscriberId();

				if (did != null && !did.equals("")) {
					uid += did;
				}
				if (sid != null && !sid.equals("")) {
					uid += "_" + sid;
				}
				if (uid.equals("")) {
					uid = "unknown";
				}
			} catch (Exception e) {

			}
		}
		return uid;
	}

	public static int getAppVersionCode(Context context, String filePath) {
		int versionCode = 0;
		if (context != null) {
			try {
				PackageInfo pi = context.getPackageManager()
						.getPackageArchiveInfo(filePath, 0);
				versionCode = pi.versionCode;
			} catch (Exception e) {

			}
		}
		return versionCode;
	}

	public static int getAppVersionCode(Context context) {
		return getAppVersionCode(context, (ApplicationInfo) null);
	}

	public static int getAppVersionCode(Context context, ApplicationInfo info) {
		int versionCode = 0;
		if (context != null) {
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
		}
		return versionCode;
	}

	public static String getAppVersionName(Context context) {
		return getAppVersionName(context, null);
	}

	public static String getAppVersionName(Context context, ApplicationInfo info) {
		String versionName = "";
		if (context != null) {
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
		}
		return versionName;
	}
}
