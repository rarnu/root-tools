package com.rarnu.tools.root.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PackageParser;
import android.content.pm.ResolveInfo;
import android.content.res.AssetManager;
import android.content.res.Resources;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Handler;
import android.os.Message;
import android.util.DisplayMetrics;

import com.rarnu.command.CommandCallback;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.common.SysappInfo;

public class ApkUtils {

	public static final int INSTALL_AUTO = 0;
	public static final int INSTALL_INTERNAL = 1;
	public static final int INSTALL_SDCARD = 2;

	public static List<SysappInfo> getSystemApps(Context context) {
		List<SysappInfo> res = new ArrayList<SysappInfo>();
		List<PackageInfo> packs = GlobalInstance.pm.getInstalledPackages(0);
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
				true, null);
		if (result.error.equals("")) {
			ret = result.result;
			try {
				ret = ret.substring(0, ret.indexOf('\t'));
			} catch (Exception e) {
				ret = "unknown";
			}
		}
		return ret;
	}

	public static boolean backupSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		CommandResult result = RootUtils.runCommand("busybox cp " + fn + " "
				+ DirHelper.SYSAPP_DIR, true, null);
		return result.error.equals("");
	}

	public static boolean deleteSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		CommandResult result = RootUtils.runCommand("rm " + fn, true, null);
		return result.error.equals("");
	}

	public static boolean deleteSystemAppData(String ns) {
		CommandResult result = RootUtils.runCommand("rm -r " + ns, true, null);
		return result.error.equals("");
	}

	public static boolean installSystemApp(String path) {
		String fn = path.substring(0, path.length() - 3) + "*";
		String onlyApkName = path.substring(path.lastIndexOf("/") + 1);
		CommandResult result = RootUtils.runCommand("busybox cp -r " + fn
				+ " /system/app/", true, null);
		if (result.error.equals("")) {
			result = RootUtils.runCommand("chmod 644 /system/app/"
					+ onlyApkName, true, null);
		}
		return result.error.equals("");
	}

	public static boolean installApp(DataappInfo info) {
		return installApp(info.localPath);
	}

	public static boolean installApp(String filePath) {
		CommandResult result = RootUtils.runCommand(
				"pm install -r " + filePath, true);
		return result.result.toLowerCase().contains("success");
	}

	public static boolean forceInstallApp(Context context, DataappInfo info) {
		boolean ret = false;
		try {
			ApplicationInfo newinfo = ApkUtils
					.getAppInfoFromPackage(info.localPath);
			String packageName = newinfo.packageName;

			backupData(context, info.localPath, packageName,
					DirHelper.FORCE_UPDATE_DIR, info);
			RootUtils.runCommand("pm uninstall " + packageName, true);
			restoreData(context, packageName, DirHelper.FORCE_UPDATE_DIR, info);
			RootUtils.runCommand("rm -r " + DirHelper.FORCE_UPDATE_DIR
					+ packageName + "*", true);
			ret = true;
		} catch (Exception e) {

		}
		return ret;
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
		return getLabelFromPackage(context, info, DirHelper.DATAAPP_DIR
				+ info.packageName + ".apk");
	}

	public static String getLabelFromPackage(Context context,
			ApplicationInfo info, String fileName) {
		Resources res = context.getResources();
		AssetManager assetMag = new AssetManager();
		assetMag.addAssetPath(fileName);
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
		return getIconFromPackage(context, info, DirHelper.DATAAPP_DIR
				+ info.packageName + ".apk");
	}

	public static Drawable getIconFromPackage(Context context,
			ApplicationInfo info, String fileName) {

		Resources res = context.getResources();
		if (info == null) {
			return res.getDrawable(android.R.drawable.sym_def_app_icon);
		}
		AssetManager assmgr = new AssetManager();
		assmgr.addAssetPath(fileName);
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

	public static void backupData(Context context, String apk, String path,
			String savePath, DataappInfo info) {

		if (savePath == null) {
			savePath = DirHelper.DATAAPP_DIR;
		}
		info.type = 1;
		String apkName = String.format(savePath + "%s.apk", path);
		File apkFile = new File(apkName);
		if (apkFile.exists()) {
			if (!GlobalInstance.overrideBackuped) {
				info.log = context.getResources().getString(
						R.string.backup_exists);
				info.logId = 1;
				operationLog.add(info);
				return;
			} else {
				String delCmd = String
						.format("rm -r " + savePath + "%s*", path);
				RootUtils.runCommand(delCmd, true, null);
			}
		}

		// delete cache before backup
		String cmd = String.format("rm -r /data/data/%s/cache", path);
		RootUtils.runCommand(cmd, true, null);

		cmd = String.format("busybox cp -r /data/data/%s " + savePath, path);
		CommandResult result = RootUtils.runCommand(cmd, true, null);

		cmd = String.format("busybox find " + savePath
				+ "%s/ -name \"cache\" | busybox xargs rm -r", path);
		RootUtils.runCommand(cmd, true, null);
		cmd = String.format("busybox find " + savePath
				+ "%s/ -name \"lib\" | busybox xargs rm -r", path);
		RootUtils.runCommand(cmd, true, null);
		cmd = String.format("busybox find " + savePath
				+ "%s/ -name \"webview*\" | busybox xargs rm -r", path);
		RootUtils.runCommand(cmd, true, null);
		cmd = String.format("busybox cp %s " + savePath + "%s.apk", apk, path);
		result = RootUtils.runCommand(cmd, true, null);

		if (result.error.equals("")) {
			info.log = context.getResources().getString(R.string.backup_ok);
			info.logId = 0;
			operationLog.add(info);

		} else {
			info.log = context.getResources().getString(R.string.backup_fail);
			info.logId = 2;
			operationLog.add(info);
		}
	}

	public static void backupData(Context context, String apk, String path,
			DataappInfo info) {
		backupData(context, apk, path, null, info);
	}

	public static void restoreData(Context context, String packageName,
			String savePath, DataappInfo info) {
		if (savePath == null) {
			savePath = DirHelper.DATAAPP_DIR;
		}
		info.type = 2;
		String cmd = String.format("pm install -r " + savePath + "%s.apk",
				packageName);
		CommandResult result = null;
		if (GlobalInstance.reinstallApk) {
			try {
				result = RootUtils.runCommand(cmd, true, null);
			} catch (Throwable th) {
				result = new CommandResult();
				result.result = "error";
			}
		} else {
			result = new CommandResult();
			result.result = "success";
		}

		if (result.result.toLowerCase().equals("success")) {
			cmd = String.format("busybox cp -r " + savePath + "%s /data/data/",
					packageName);
			result = RootUtils.runCommand(cmd, true, null);
			if (result.error.equals("")) {

				cmd = String.format("busybox chmod -R 777 /data/data/%s/*",
						packageName);
				result = RootUtils.runCommand(cmd, true, null);
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

	public static void restoreData(Context context, String packageName,
			DataappInfo info) {
		restoreData(context, packageName, null, info);
	}

	public static void deleteBackupData(String packageName) {
		String cmd = String.format("busybox rm -r " + DirHelper.DATAAPP_DIR
				+ "%s*", packageName);
		RootUtils.runCommand(cmd, true, null);
	}

	public static void deleteAllBackupData() {
		RootUtils.runCommand("busybox rm -r " + DirHelper.DATAAPP_DIR + "*",
				true, null);
	}

	public static boolean uninstallApk(String packageName) {
		try {
			CommandResult cmdRet = RootUtils.runCommand(
					String.format("pm uninstall %s", packageName), true, null);
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

	public static void gotoApp(Context context, String namespace, String url) {
		if (ApkUtils.applicationInstalled(namespace)) {
			openApp(context, namespace);
			// ApkUtils.startApplication(namespace, activity);
		} else {
			openDownloadApp(context, url);
		}
	}

	private static final String PACKAGE_URL = "http://rarnu.7thgen.info/root_tools/package/";

	public static void openDownloadApp(Context context, String url) {
		String downloadUrl = PACKAGE_URL + url;
		Intent inDownload = new Intent(Intent.ACTION_VIEW);
		inDownload.setData(Uri.parse(downloadUrl));
		context.startActivity(inDownload);
	}

	public static void openGooglePlayForApp(Context context, String namespace) {
		Intent inPlay = new Intent(Intent.ACTION_VIEW);
		inPlay.setData(Uri.parse("market://details?id=" + namespace));
		context.startActivity(inPlay);
	}

	public static void setInstallLocation(int location) {
		RootUtils.runCommand(
				"pm set-install-location " + String.valueOf(location), true,
				null);
	}

	public static void openApp(Context context, String packageName) {
		PackageInfo pi = null;
		try {
			pi = GlobalInstance.pm.getPackageInfo(packageName, 0);
		} catch (NameNotFoundException e) {
		}

		if (pi == null) {
			return;
		}

		Intent resolveIntent = new Intent(Intent.ACTION_MAIN, null);
		resolveIntent.addCategory(Intent.CATEGORY_LAUNCHER);
		resolveIntent.setPackage(pi.packageName);

		List<ResolveInfo> apps = GlobalInstance.pm.queryIntentActivities(
				resolveIntent, 0);

		ResolveInfo ri = apps.iterator().next();
		if (ri != null) {
			String className = ri.activityInfo.name;
			Intent intent = new Intent(Intent.ACTION_MAIN);
			intent.addCategory(Intent.CATEGORY_LAUNCHER);
			ComponentName cn = new ComponentName(packageName, className);
			intent.setComponent(cn);
			context.startActivity(intent);
		}
	}

	public static void scanApksInSdcard(final CommandCallback callback) {
		new Thread(new Runnable() {

			@Override
			public void run() {
				String cmd = "busybox find /sdcard/ -name \"*.apk\"";
				RootUtils.runCommand(cmd, true, callback);
			}
		}).start();
	}

	/**
	 * getApkFileStatus
	 * 
	 * @param newinfo
	 * @return status with the new application info<br>
	 *         return 0: installed with same signature<br>
	 *         return 1: installed with different signature<br>
	 *         return 2: no need update<br>
	 *         return 3: not installed
	 */
	public static int getApkFileStatus(Context context, DataappInfo newinfo) {
		String packageName = newinfo.info.packageName;
		ApplicationInfo installedInfo = null;
		try {
			installedInfo = GlobalInstance.pm
					.getApplicationInfo(packageName, 0);
		} catch (NameNotFoundException e) {

		}
		if (installedInfo == null) {
			return 3;
		}
		int newVer = DeviceUtils.getAppVersionCode(context, newinfo.localPath);
		int oldVer = DeviceUtils.getAppVersionCode(context, installedInfo);
		if (newVer <= oldVer) {
			return 2;
		}
		int compare = GlobalInstance.pm.checkSignatures(newinfo.info.uid,
				installedInfo.uid);
		return (compare == PackageManager.SIGNATURE_MATCH ? 0 : 1);
	}
}
