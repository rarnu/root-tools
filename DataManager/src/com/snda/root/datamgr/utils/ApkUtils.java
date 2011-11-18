package com.snda.root.datamgr.utils;

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

import com.snda.root.datamgr.GlobalInstance;
import com.snda.root.datamgr.R;

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

	public static void backupData(Context context, String name, String apk,
			String path, AppInfo info) {
		info.type = 1;
		String apkName = String.format("/sdcard/.app_data/%s.apk", path);
		File apkFile = new File(apkName);
		if (apkFile.exists()) {
			if (!GlobalInstance.pref_override) {
				info.log = context.getResources().getString(
						R.string.backup_exists);
				info.logId = 1;
				operationLog.add(info);
				return;
			} else {
				String delCmd = String.format("rm -r /sdcard/.app_data/%s*",
						path);
				RootUtils.runRootCommand(delCmd);
			}
		}

		String cmd = String.format(
				"busybox cp -r /data/data/%s /sdcard/.app_data/", path);
		CommandResult result = RootUtils.runRootCommand(cmd);
		if (result.error.equals("")) {

			cmd = String
					.format(
							"busybox find /sdcard/.app_data/%s/ -name \"cache\" | busybox xargs rm -r",
							path);
			RootUtils.runRootCommand(cmd);
			cmd = String
					.format(
							"busybox find /sdcard/.app_data/%s/ -name \"lib\" | busybox xargs rm -r",
							path);
			RootUtils.runRootCommand(cmd);
			cmd = String
					.format(
							"busybox find /sdcard/.app_data/%s/ -name \"webview*\" | busybox xargs rm -r",
							path);
			RootUtils.runRootCommand(cmd);
			cmd = String.format("busybox cp %s /sdcard/.app_data/%s.apk", apk,
					path);
			RootUtils.runRootCommand(cmd);

			info.log = context.getResources().getString(R.string.backup_ok);
			info.logId = 0;
			operationLog.add(info);

		} else {
			info.log = context.getResources().getString(R.string.backup_fail);
			info.logId = 2;
			operationLog.add(info);
		}
	}

	public static void restoreData(Context context, String name,
			String packageName, AppInfo info) {
		info.type = 2;
		String cmd = String.format("pm install -r /sdcard/.app_data/%s.apk",
				packageName);
		CommandResult result = null;
		if (GlobalInstance.pref_apk) {
			result = RootUtils.runRootCommand(cmd);
		} else {
			result = new CommandResult();
			result.result = "Success";
		}

		if (result.result.toLowerCase().equals("success")) {
			cmd = String.format(
					"busybox cp -r /sdcard/.app_data/%s /data/data/",
					packageName);
			result = RootUtils.runRootCommand(cmd);
			if (result.error.equals("")) {

				cmd = String.format("busybox chmod -R 777 /data/data/%s/*",
						packageName);
				result = RootUtils.runRootCommand(cmd);
				if (result.error.equals("")) {
					info.log = context.getResources().getString(
							R.string.restore_ok);
					info.logId = 0;
					operationLog.add(info);
				} else {
					info.log = context.getResources().getString(
							R.string.restore_fail);
					info.logId = 2;
					operationLog.add(info);
				}
			} else {
				info.log = context.getResources().getString(
						R.string.restore_fail);
				info.logId = 2;
				operationLog.add(info);
			}
		} else {
			info.log = context.getResources().getString(R.string.restore_fail);
			info.logId = 2;
			operationLog.add(info);
		}
	}

	public static void deleteBackupData(String packageName) {
		String cmd = String.format("rm -r /sdcard/.app_data/%s*", packageName);
		RootUtils.runRootCommand(cmd);
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

	public static boolean installSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		CommandResult result = RootUtils.runRootCommand("cp -r " + fn
				+ " /system/app/");
		return result.error.equals("");
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
		assetMag.addAssetPath("/sdcard/.app_data/" + info.packageName + ".apk");
		res = new Resources(assetMag, res.getDisplayMetrics(), res
				.getConfiguration());
		if (info.labelRes != 0) {
			return res.getText(info.labelRes).toString();
		} else {
			return context.getResources().getString(R.string.no_name);
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
