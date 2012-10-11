package com.rarnu.findaround.common;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.net.Uri;

import com.rarnu.findaround.GlobalInstance;

public class AppUtils {

	public static boolean applicationInstalled(String namespace) {
		try {
			PackageInfo info = GlobalInstance.pm.getPackageInfo(namespace, 0);
			return info != null;
		} catch (NameNotFoundException e) {
			return false;
		}
	}

	public static boolean startApplication(String namespace, String activity) {
		try {
			String cmd = "am start -a android.intent.action.MAIN -c android.intent.category.LAUNCHER -n %s/%s";
			Runtime.getRuntime().exec(String.format(cmd, namespace, activity));
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	public static void gotoApp(Context context, String namespace,
			String activity) {
		if (applicationInstalled(namespace)) {
			startApplication(namespace, activity);
		} else {
			openGooglePlayForApp(context, namespace);
		}
	}

	public static void openGooglePlayForApp(Context context, String namespace) {
		Intent inPlay = new Intent(Intent.ACTION_VIEW);
		inPlay.setData(Uri.parse("market://details?id=" + namespace));
		context.startActivity(inPlay);
	}
}
