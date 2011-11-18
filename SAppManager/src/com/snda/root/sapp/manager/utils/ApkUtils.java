package com.snda.root.sapp.manager.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageParser;
import android.content.res.AssetManager;
import android.content.res.Resources;
import android.graphics.drawable.Drawable;
import android.util.DisplayMetrics;

import com.snda.root.sapp.manager.R;
import com.snda.root.sapp.manager.adapter.AppInfo;

public class ApkUtils {

	public static List<AppInfo> getSystemApps(Context context) {
		List<AppInfo> res = new ArrayList<AppInfo>();
		List<PackageInfo> packs = context.getPackageManager()
				.getInstalledPackages(0);
		for (int i = 0; i < packs.size(); i++) {
			PackageInfo p = packs.get(i);
			ApplicationInfo newInfo = p.applicationInfo;
			if (newInfo.sourceDir.contains("/system/app")) {
				AppInfo info = new AppInfo();
				info.info = newInfo;
				info.level = getAppLevel(newInfo.sourceDir);
				res.add(info);
			}
		}
		return res;
	}

	public static String getAppSize(String path) {

		int fileLen = getFileSize(path);

		String odexPath = path.substring(0, path.length() - 3) + "odex";
		File fOdex = new File(odexPath);
		if (fOdex.exists()) {
			fileLen += getFileSize(odexPath);
		}

		return new DecimalFormat("#.##").format(fileLen / 1024);
	}

	private static int getFileSize(String path) {
		File f = new File(path);
		int fileLen = 0;

		FileInputStream fis = null;
		try {
			fis = new FileInputStream(f);
			fileLen = fis.available();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (fis != null) {
				try {
					fis.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		return fileLen;
	}

	public static String getDataSize(String path) {
		String ret = "";
		CommandResult result = RootUtils.runRootCommand("du -s " + path);
		if (result.error.equals("")) {
			ret = result.result;
			ret = ret.substring(0, ret.indexOf('\t'));
		}
		return ret;
	}

	public static boolean backupSystemApp() {
		CommandResult result = RootUtils
				.runRootCommand("cp -r /system/app/* /sdcard/.sapp");
		return result.error.equals("");
	}

	public static boolean deleteSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		CommandResult result = RootUtils.runRootCommand("rm " + fn);
		return result.error.equals("");
	}

	public static boolean installSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		CommandResult result = RootUtils.runRootCommand("cp -r " + fn
				+ " /system/app/");
		return result.error.equals("");
	}

	public static boolean isAndroidApp(String path) {
		boolean ret = false;
		for (String s : AppNameConst.systemApps) {
			if (path.toLowerCase().equals(s.toLowerCase())) {
				ret = true;
				break;
			}
		}
		return ret;
	}

	public static boolean isGoogleApp(String path) {
		boolean ret = false;
		for (String s : AppNameConst.googleApps) {
			if (path.toLowerCase().equals(s.toLowerCase())) {
				ret = true;
				break;
			}
		}
		return ret;
	}

	public static int getAppLevel(String path) {
		File fApk = new File(path);
		String apkName = fApk.getName();

		int applevel = 2;
		if (ApkUtils.isAndroidApp(apkName)) {
			applevel = 0;
		}
		if (applevel == 2) {
			if (ApkUtils.isGoogleApp(apkName)) {
				applevel = 1;
			}
		}
		return applevel;
	}

	public static Drawable getIconFromPackage(Context context, String archiveFilePath) {
		PackageParser packageParser = new PackageParser(archiveFilePath);
		File sourceFile = new File(archiveFilePath);
		DisplayMetrics metrics = new DisplayMetrics();
		metrics.setToDefaults();
		PackageParser.Package pkg = packageParser.parsePackage(sourceFile,
				archiveFilePath, metrics, 0);
		if (pkg == null)
			return context.getResources().getDrawable(R.drawable.android);

		ApplicationInfo info = pkg.applicationInfo;
		
		Resources pRes = context.getResources();
		AssetManager assmgr = new AssetManager();
		assmgr.addAssetPath(archiveFilePath);
		Resources res = new Resources(assmgr, pRes.getDisplayMetrics(), pRes
				.getConfiguration());

		if (info.icon != 0) {
			Drawable icon = res.getDrawable(info.icon);
			return icon;
		} else {
			return context.getResources().getDrawable(R.drawable.android);
		}
	}
}
