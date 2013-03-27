package com.rarnu.almanac;

import android.content.Context;
import android.content.pm.PackageInfo;

public class DeviceUtils {

	public static int getAppVersionCode(Context context) {
		int versionCode = 0;
		try {
			PackageInfo pi = context.getPackageManager().getPackageInfo(
					context.getPackageName(), 0);
			versionCode = pi.versionCode;

		} catch (Exception e) {

		}
		return versionCode;
	}

	public static String getAppVersionName(Context context) {
		String versionName = "";
		try {
			PackageInfo pi = context.getPackageManager().getPackageInfo(
					context.getPackageName(), 0);
			versionName = pi.versionName;

		} catch (Exception e) {

		}
		return versionName;
	}

}
