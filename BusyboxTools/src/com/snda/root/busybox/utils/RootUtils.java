package com.snda.root.busybox.utils;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;

import android.util.Log;

public class RootUtils {

	private static final String SU_PATH = "/system/bin/su";
	private static final String SU_PATH_X = "/system/xbin/su";
	private static final String BUSYBOX_PATH = "/system/xbin/busybox";
	private static final String APP_PATH = "/system/app/";

	/**
	 * 
	 * @return |0:no root|1:find only su|2:find two|
	 */
	public static int hasRoot() {
		boolean hasSU = findSU();
		if (!hasSU) {
			return 0;
		}
		boolean hasSuperUser = findSuperUser();
		return hasSuperUser ? 2 : 1;
	}
	
	public static boolean hasBusybox() {
		return openFile(BUSYBOX_PATH).exists();
	}

	public static CommandResult runRootCommand(String command) {
		Log.e("runRootCommand", command);
		Process process = null;
		DataOutputStream os = null;
		DataInputStream stdout = null;
		DataInputStream stderr = null;
		CommandResult ret = new CommandResult();
		try {
			StringBuffer output = new StringBuffer();
			StringBuffer error = new StringBuffer();
			process = Runtime.getRuntime().exec("su");
			os = new DataOutputStream(process.getOutputStream());
			os.writeBytes(command + "\n");
			os.writeBytes("exit\n");
			os.flush();
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
				process.destroy();
			} catch (Exception e) {
				ret.result = "";
				ret.error = e.getMessage();
			}
		}
		return ret;
	}

	public static void runCommand(String command) {
		try {
			Runtime.getRuntime().exec(command);
		} catch (IOException e) {
			Log.e("runCommand", e.getMessage());
		}
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

	private static File openFile(String path) {
		return new File(path);
	}

	public static String buildMountCommand() {
		String retstr = "";
		CommandResult ret = runRootCommand("mount");
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

}
