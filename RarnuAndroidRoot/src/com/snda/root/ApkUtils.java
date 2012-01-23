package com.snda.root;

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

public class ApkUtils {

	private static List<AppInfo> operationLog = new ArrayList<AppInfo>();

	public static void clearOperationLog() {
		operationLog.clear();
	}

	public static List<AppInfo> getOperationLog() {
		return operationLog;
	}

	public static List<AppInfo> getInstalledApps(Context context) {
		List<AppInfo> res = new ArrayList<AppInfo>();
		List<PackageInfo> packs = GlobalInstance.pm.getInstalledPackages(0);
		for (int i = 0; i < packs.size(); i++) {
			PackageInfo p = packs.get(i);
			ApplicationInfo newInfo = p.applicationInfo;
			if (newInfo.sourceDir.contains("/system/app/")
					|| newInfo.sourceDir.contains("/data/app/")) {
				AppInfo info = new AppInfo();
				info.info = newInfo;
				info.checked = false;
				res.add(info);
			}
		}
		return res;
	}

	public static List<AppInfo> getBackupedApps(Context context) {
		List<AppInfo> res = new ArrayList<AppInfo>();
		File fBackupDir = new File("/sdcard/.app_data/");
		if (fBackupDir.exists()) {
			for (String s : fBackupDir.list()) {
				if (s.toLowerCase().endsWith(".apk")) {
					AppInfo newinfo = new AppInfo();
					newinfo.info = getAppInfoFromPackage("/sdcard/.app_data/"
							+ s);
					newinfo.checked = false;
					res.add(newinfo);
				}
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

	

	public static ApplicationInfo getAppInfoFromPackage(String filePath) {
		PackageParser packageParser = new PackageParser(filePath);
		File sourceFile = new File(filePath);
		DisplayMetrics metrics = new DisplayMetrics();
		metrics.setToDefaults();
		PackageParser.Package pkg = packageParser.parsePackage(sourceFile,
				filePath, metrics, PackageParser.PARSE_ON_SDCARD);
		if (pkg == null) {
			return null;
		}

		ApplicationInfo info = pkg.applicationInfo;
		return info;
	}

	public static String getLabelFromPackage(Context context,
			ApplicationInfo info) {
		Resources res = context.getResources();
		AssetManager assetMag = new AssetManager();
		assetMag.addAssetPath(info.publicSourceDir);
		res = new Resources(assetMag, res.getDisplayMetrics(), res
				.getConfiguration());
		if (info.labelRes != 0) {
			return res.getText(info.labelRes).toString();
		} else {
			return info.packageName;
			// return context.getResources().getString(R.string.no_name);
		}
	}

	public static Drawable getIconFromPackage(Context context,
			ApplicationInfo info) {

		Resources res = context.getResources();
		AssetManager assmgr = new AssetManager();
		assmgr.addAssetPath("/sdcard/.app_data/" + info.packageName + ".apk");
		res = new Resources(assmgr, res.getDisplayMetrics(), res
				.getConfiguration());

		if (info.icon != 0) {
			return res.getDrawable(info.icon);
		} else {
			return res.getDrawable(android.R.drawable.sym_def_app_icon);
		}
	}
}
