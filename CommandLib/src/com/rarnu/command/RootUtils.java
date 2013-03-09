package com.rarnu.command;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;

public class RootUtils {

	private static final String SU_PATH = "/system/bin/su";
	private static final String SU_PATH_X = "/system/xbin/su";
	private static final String APP_PATH = "/system/app/";
	private static final String BUSYBOX_PATH = "/system/xbin/busybox";

	private static final String[] SUPERUSER_PATH = new String[] {
			"eu.chainfire.supersu", "eu.chainfire.supersu.pro",
			"com.noshufou.android.su", "com.miui.uac",
			"com.lbe.security.shuame", "com.lbe.security.miui", "com.m0narx.su" };

	private static final String SETTINGS_PACKAGE = "com.android.settings";

	private static PackageManager pm = null;

	public static void init(Context context) {
		pm = context.getPackageManager();
	}

	public static boolean hasBusybox() {
		return openFile(BUSYBOX_PATH).exists();
	}

	public static boolean hasSuperuser() {

		boolean ret = false;
		for (int i = 0; i < SUPERUSER_PATH.length; i++) {
			ret = applicationExists(SUPERUSER_PATH[i]);
			if (ret) {
				break;
			}
		}

		if (!ret) {
			ret = isSettingsContainsSU();
		}

		return ret;
	}

	private static boolean applicationExists(String packageName) {
		ApplicationInfo info = null;
		try {
			info = pm.getApplicationInfo(packageName, 0);
		} catch (NameNotFoundException e) {
			info = null;
		}
		return info != null;
	}

	public static int hasRoot() {
		boolean hasSU = findSU();
		if (!hasSU) {
			return 0;
		}
		boolean hasSuperUser = findSuperUser();
		return hasSuperUser ? 2 : 1;
	}

	public static boolean isWrongRoot() {
		// -rwsr-sr-x
		String suStat = runCommand("ls -l " + SU_PATH, false).result
				+ runCommand("ls -l " + SU_PATH_X, false).result;

		return ((!suStat.contains("-rwsr-sr-x")) && (!suStat
				.contains("-rwsr-xr-x")));
	}

	public static CommandResult runCommand(String command, boolean root) {

		Process process = null;
		DataOutputStream os = null;
		DataInputStream stdout = null;
		DataInputStream stderr = null;
		CommandResult ret = new CommandResult();
		try {
			StringBuffer output = new StringBuffer();
			StringBuffer error = new StringBuffer();
			if (root) {
				process = Runtime.getRuntime().exec("su");
				os = new DataOutputStream(process.getOutputStream());
				os.writeBytes(command + "\n");
				os.writeBytes("exit\n");
				os.flush();
			} else {
				process = Runtime.getRuntime().exec(command);
			}
			stdout = new DataInputStream(process.getInputStream());
			String line;
			while ((line = stdout.readLine()) != null) {
				output.append(line).append('\n');
			}
			stderr = new DataInputStream(process.getErrorStream());
			while ((line = stderr.readLine()) != null) {
				error.append(line).append('\n');
			}
			process.waitFor();
			ret.result = output.toString().trim();
			ret.error = error.toString().trim();
		} catch (Exception e) {
			ret.result = "";
			ret.error = e.getMessage();
		} finally {
			try {
				if (os != null) {
					os.close();
				}
				if (stdout != null) {
					stdout.close();
				}
				if (stderr != null) {
					stderr.close();
				}
			} catch (Exception e) {
				ret.result = "";
				ret.error = e.getMessage();
			}
		}

		return ret;
	}

	private static boolean findSuperUser() {
		File apps = new File(APP_PATH);
		String[] apks = apps.list();
		boolean hasSuperUser = false;
		if (apks != null) {
			if (apks.length > 0) {
				for (String apk : apks) {
					if (apk.toLowerCase().contains("superuser.apk")) {
						hasSuperUser = true;
						break;
					}
				}
			}
		}
		return hasSuperUser;
	}

	private static boolean findSU() {
		boolean ret = openFile(SU_PATH).exists();
		if (!ret) {
			ret = openFile(SU_PATH_X).exists();
		}
		return ret;
	}

	public static boolean hasSu() {
		return findSU();
	}

	private static File openFile(String path) {
		return new File(path);
	}

	public static String buildMountCommand() {
		String retstr = "";
		CommandResult ret = runCommand("mount", false);
		if (ret.error.equals("")) {
			String[] mt = ret.result.split("\n");
			for (String m : mt) {
				if (m.contains("/system")) {
					String mstr = m;
					mstr = mstr.replace(" on ", " ").trim();
					String[] mele = mstr.split(" ");
					int cnt = 0;
					for (String me : mele) {
						if (cnt >= 2) {
							break;
						}
						if (!me.trim().equals("")) {
							retstr = retstr + " " + me;
							cnt++;
						}
					}
					break;
				}
			}
		}
		if (!retstr.equals("")) {
			retstr = "mount -o remount,rw" + retstr;
		}
		return retstr;
	}

	public static void mountRW() {
		String cmd = buildMountCommand();
		runCommand(cmd, true);
	}

	private static boolean isSettingsContainsSU() {
		boolean ret = false;
		try {
			PackageInfo pi = pm.getPackageInfo(SETTINGS_PACKAGE, 0);
			String versionName = pi.versionName;
			versionName = versionName
					.substring(0, versionName.lastIndexOf("."));
			versionName = versionName
					.substring(versionName.lastIndexOf(".") + 1);
			ret = (Integer.parseInt(versionName) >= 20130308);
		} catch (Exception e) {

		}

		return ret;
	}
}
