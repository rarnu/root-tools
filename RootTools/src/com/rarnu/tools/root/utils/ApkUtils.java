package com.rarnu.tools.root.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PackageParser;
import android.content.res.AssetManager;
import android.content.res.Resources;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Handler;
import android.os.Message;
import android.util.DisplayMetrics;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.utils.root.CommandResult;
import com.rarnu.tools.root.utils.root.RootUtils;

public class ApkUtils {

	public static List<SysappInfo> getSystemApps(Context context) {
		List<SysappInfo> res = new ArrayList<SysappInfo>();
		List<PackageInfo> packs = context.getPackageManager()
				.getInstalledPackages(0);
		int position = 0;
		for (int i = 0; i < packs.size(); i++) {
			PackageInfo p = packs.get(i);
			ApplicationInfo newInfo = p.applicationInfo;
			if (newInfo == null) {
				continue;
			}
			if (newInfo.sourceDir.contains("/system/app")) {
				SysappInfo info = new SysappInfo();
				info.info = newInfo;
				info.level = getAppLevel(newInfo.sourceDir, newInfo.packageName);
				info.position = position;
				res.add(info);
				position++;
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
		CommandResult result = RootUtils.runCommand("busybox du -s " + path,
				false);
		if (result.error.equals("")) {
			ret = result.result;
			ret = ret.substring(0, ret.indexOf('\t'));
		}
		return ret;
	}

	public static boolean backupSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		CommandResult result = RootUtils.runCommand("busybox cp " + fn + " "
				+ DirHelper.SYSAPP_DIR, true);
		return result.error.equals("");
	}

	public static boolean deleteSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		CommandResult result = RootUtils.runCommand("rm " + fn, true);
		return result.error.equals("");
	}

	public static boolean deleteSystemAppData(String ns) {
		CommandResult result = RootUtils.runCommand("rm -r " + ns, true);
		return result.error.equals("");
	}

	public static boolean installSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		CommandResult result = RootUtils.runCommand("busybox cp -r " + fn
				+ " /system/app/", true);
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

	public static boolean isGoogleApp(String ns) {
		return ns.contains("com.google.");
	}

	public static boolean isHtcApp(String ns) {
		return ns.contains("com.htc.");
	}

	public static int getAppLevel(String path, String ns) {
		File fApk = new File(path);
		String apkName = fApk.getName();

		int applevel = 3;
		if (ApkUtils.isAndroidApp(apkName)) {
			applevel = 0;
		}
		if (applevel == 3) {
			if (ApkUtils.isGoogleApp(ns)) {
				applevel = 1;
			}
		}
		if (applevel == 3) {
			if (ApkUtils.isHtcApp(ns)) {
				applevel = 2;
			}
		}
		return applevel;
	}

	public static Drawable getIconFromPackage(Context context,
			String archiveFilePath) {
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
		Resources res = new Resources(assmgr, pRes.getDisplayMetrics(),
				pRes.getConfiguration());

		if (info.icon != 0) {
			Drawable icon = res.getDrawable(info.icon);
			return icon;
		} else {
			return context.getResources().getDrawable(R.drawable.android);
		}
	}

	public static void installSystemApp(final Context context,
			final String path, final Handler h) {

		new Thread(new Runnable() {

			@Override
			public void run() {
				boolean installOK = installSystemApp(path);
				try {
					Thread.sleep(2000);
				} catch (InterruptedException e) {
				}
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = (installOK ? 1 : 0);
				h.sendMessage(msg);
			}
		}).start();
	}

	public static String getLabelFromPackage(Context context,
			ApplicationInfo info) {
		Resources res = context.getResources();
		AssetManager assetMag = new AssetManager();
		assetMag.addAssetPath(DirHelper.DATAAPP_DIR + info.packageName + ".apk");
		res = new Resources(assetMag, res.getDisplayMetrics(),
				res.getConfiguration());

		try {
			if (info.labelRes != 0) {
				return res.getText(info.labelRes).toString();
			} else {
				return context.getResources().getString(R.string.no_name);
			}
		} catch (Exception e) {
			return context.getResources().getString(R.string.no_name);
		}
	}

	public static Drawable getIconFromPackage(Context context,
			ApplicationInfo info) {

		Resources res = context.getResources();
		AssetManager assmgr = new AssetManager();
		assmgr.addAssetPath(DirHelper.DATAAPP_DIR + info.packageName + ".apk");
		res = new Resources(assmgr, res.getDisplayMetrics(),
				res.getConfiguration());

		try {
			if (info.icon != 0) {
				return res.getDrawable(info.icon);
			} else {
				return res.getDrawable(android.R.drawable.sym_def_app_icon);
			}
		} catch (Exception e) {
			return res.getDrawable(android.R.drawable.sym_def_app_icon);
		}
	}

	public static String getLabelFromPackageEx(Context context,
			ApplicationInfo info) {
		Resources res = context.getResources();
		AssetManager assetMag = new AssetManager();
		assetMag.addAssetPath(info.publicSourceDir);
		res = new Resources(assetMag, res.getDisplayMetrics(),
				res.getConfiguration());
		if (info.labelRes != 0) {
			return res.getText(info.labelRes).toString();
		} else {
			return info.packageName;
			// return context.getResources().getString(R.string.no_name);
		}
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

	public static List<DataappInfo> getInstalledApps(Context context,
			boolean includeSystem) {
		List<DataappInfo> res = new ArrayList<DataappInfo>();
		List<PackageInfo> packs = GlobalInstance.pm.getInstalledPackages(0);
		int position = 0;
		for (int i = 0; i < packs.size(); i++) {
			PackageInfo p = packs.get(i);
			ApplicationInfo newInfo = p.applicationInfo;
			if (newInfo == null) {
				continue;
			}
			if ((includeSystem && newInfo.sourceDir.contains("/system/app/"))
					|| newInfo.sourceDir.contains("/data/app/")) {
				DataappInfo info = new DataappInfo();
				info.info = newInfo;
				info.checked = false;
				info.position = position;
				res.add(info);
				position++;
			}
		}
		return res;
	}

	public static List<EnableappInfo> getInstalledAppsEnabled(Context context) {
		List<EnableappInfo> res = new ArrayList<EnableappInfo>();
		List<PackageInfo> packs = GlobalInstance.pm.getInstalledPackages(0);

		for (int i = 0; i < packs.size(); i++) {
			PackageInfo p = packs.get(i);
			ApplicationInfo newInfo = p.applicationInfo;
			if (newInfo == null) {
				continue;
			}

			EnableappInfo info = new EnableappInfo();
			info.info = newInfo;
			info.enabled = true;

			if (newInfo.sourceDir.contains("/system/app/")) {
				info.type = 0;
			} else if (newInfo.sourceDir.contains("/data/app/")) {
				info.type = 1;
			} else {
				info.type = 2;
			}

			res.add(info);

		}
		return res;
	}

	public static List<EnableappInfo> getInstalledAppsDisabled(Context context) {
		List<EnableappInfo> res = new ArrayList<EnableappInfo>();
		File fDisableSystem = new File(DirHelper.ENABLEAPP_DIR_SYSTEM);
		if (fDisableSystem.exists()) {
			for (String s : fDisableSystem.list()) {
				if (s.toLowerCase().endsWith(".apk")) {
					EnableappInfo newinfo = new EnableappInfo();
					newinfo.info = getAppInfoFromPackage(DirHelper.ENABLEAPP_DIR_SYSTEM
							+ s);

					if (newinfo.info == null) {
						continue;
					}
					newinfo.type = 0;
					newinfo.enabled = false;
					newinfo.filePath = DirHelper.ENABLEAPP_DIR_SYSTEM + s;
					res.add(newinfo);
				}
			}
		}

		File fDisableData = new File(DirHelper.ENABLEAPP_DIR_DATA);
		if (fDisableData.exists()) {
			for (String s : fDisableData.list()) {
				if (s.toLowerCase().endsWith(".apk")) {
					EnableappInfo newinfo = new EnableappInfo();
					newinfo.info = getAppInfoFromPackage(DirHelper.ENABLEAPP_DIR_DATA
							+ s);

					if (newinfo.info == null) {
						continue;
					}
					newinfo.type = 1;
					newinfo.enabled = false;
					newinfo.filePath = DirHelper.ENABLEAPP_DIR_DATA + s;
					res.add(newinfo);
				}
			}
		}

		return res;
	}

	public static List<EnableappInfo> getEnabledApplications(Context context) {
		List<EnableappInfo> listEnabled = getInstalledAppsEnabled(context);
		List<EnableappInfo> listDisabled = getInstalledAppsDisabled(context);
		listDisabled.addAll(listEnabled);
		return listDisabled;
	}

	public static List<DataappInfo> getBackupedApps(Context context) {
		List<DataappInfo> res = new ArrayList<DataappInfo>();
		File fBackupDir = new File(DirHelper.DATAAPP_DIR);
		int position = 0;
		if (fBackupDir.exists()) {
			for (String s : fBackupDir.list()) {
				if (s.toLowerCase().endsWith(".apk")) {
					DataappInfo newinfo = new DataappInfo();
					newinfo.info = getAppInfoFromPackage(DirHelper.DATAAPP_DIR
							+ s);
					newinfo.checked = false;
					newinfo.position = position;
					if (newinfo.info == null) {
						continue;
					}
					res.add(newinfo);
					position++;
				}
			}
		}
		return res;
	}

	public static void backupData(Context context, String name, String apk,
			String path, DataappInfo info) {
		info.type = 1;
		String apkName = String.format(DirHelper.DATAAPP_DIR + "%s.apk", path);
		File apkFile = new File(apkName);
		if (apkFile.exists()) {
			if (!GlobalInstance.overrideBackuped) {
				info.log = context.getResources().getString(
						R.string.backup_exists);
				info.logId = 1;
				operationLog.add(info);
				return;
			} else {
				String delCmd = String.format("rm -r " + DirHelper.DATAAPP_DIR
						+ "%s*", path);
				RootUtils.runCommand(delCmd, true);
			}
		}

		// delete cache before backup
		String cmd = String.format("rm -r /data/data/%s/cache", path);
		RootUtils.runCommand(cmd, true);

		cmd = String.format("busybox cp -r /data/data/%s "
				+ DirHelper.DATAAPP_DIR, path);
		CommandResult result = RootUtils.runCommand(cmd, true);
		if (result.error.equals("")) {

			cmd = String.format("busybox find " + DirHelper.DATAAPP_DIR
					+ "%s/ -name \"cache\" | busybox xargs rm -r", path);
			RootUtils.runCommand(cmd, true);
			cmd = String.format("busybox find " + DirHelper.DATAAPP_DIR
					+ "%s/ -name \"lib\" | busybox xargs rm -r", path);
			RootUtils.runCommand(cmd, true);
			cmd = String.format("busybox find " + DirHelper.DATAAPP_DIR
					+ "%s/ -name \"webview*\" | busybox xargs rm -r", path);
			RootUtils.runCommand(cmd, true);
			cmd = String.format("busybox cp %s " + DirHelper.DATAAPP_DIR
					+ "%s.apk", apk, path);
			RootUtils.runCommand(cmd, true);

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
			String packageName, DataappInfo info) {
		info.type = 2;
		String cmd = String.format("pm install -r " + DirHelper.DATAAPP_DIR
				+ "%s.apk", packageName);
		CommandResult result = null;
		if (GlobalInstance.reinstallApk) {
			try {
				result = RootUtils.runCommand(cmd, true);
			} catch (Throwable th) {
				result = new CommandResult();
				result.result = "error";
			}
		} else {
			result = new CommandResult();
			result.result = "success";
		}

		if (result.result.toLowerCase().equals("success")) {
			cmd = String.format("busybox cp -r " + DirHelper.DATAAPP_DIR
					+ "%s /data/data/", packageName);
			result = RootUtils.runCommand(cmd, true);
			if (result.error.equals("")) {

				cmd = String.format("busybox chmod -R 777 /data/data/%s/*",
						packageName);
				result = RootUtils.runCommand(cmd, true);
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
		String cmd = String.format("busybox rm -r " + DirHelper.DATAAPP_DIR
				+ "%s*", packageName);
		RootUtils.runCommand(cmd, true);
	}

	public static void deleteAllBackupData() {
		RootUtils.runCommand("busybox rm -r " + DirHelper.DATAAPP_DIR + "*",
				true);
	}

	public static boolean uninstallApk(String packageName) {
		try {
			CommandResult cmdRet = RootUtils.runCommand(String.format("pm uninstall %s", packageName), true);
			return cmdRet.error.equals("");
		} catch (Exception e) {
			return false;
		}
	}
	
	private static List<DataappInfo> operationLog = new ArrayList<DataappInfo>();

	public static void clearOperationLog() {
		operationLog.clear();
	}

	public static List<DataappInfo> getOperationLog() {
		return operationLog;
	}

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
		if (ApkUtils.applicationInstalled(namespace)) {
			ApkUtils.startApplication(namespace, activity);
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
