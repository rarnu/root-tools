package com.rarnu.tools.root.utils.root;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.util.Log;

import com.rarnu.tools.root.GlobalInstance;

public class RootUtils {

	private static final String SU_PATH = "/system/bin/su";
	private static final String SU_PATH_X = "/system/xbin/su";
	private static final String APP_PATH = "/system/app/";
	private static final String BUSYBOX_PATH = "/system/xbin/busybox";
	private static final String SUPERUSER_PATH_1 = "eu.chainfire.supersu";
	private static final String SUPERUSER_PATH_2 = "eu.chainfire.supersu.pro";
	private static final String SUPERUSER_PATH_3 = "com.noshufou.android.su";
	private static final String SUPERUSER_PATH_4 = "com.miui.uac";
	private static final String SUPERUSER_PATH_5 = "com.lbe.security.shuame";

	public static boolean hasBusybox() {
		return openFile(BUSYBOX_PATH).exists();
	}

	public static boolean hasSuperuser() {

		ApplicationInfo info = null;
		try {
			info = GlobalInstance.pm.getApplicationInfo(SUPERUSER_PATH_1, 0);
		} catch (NameNotFoundException e) {
			info = null;
		}

		if (info == null) {
			try {
				info = GlobalInstance.pm.getApplicationInfo(SUPERUSER_PATH_2, 0);
			} catch (NameNotFoundException e) {
				info = null;
			}
		}

		if (info == null) {
			try {
				info = GlobalInstance.pm.getApplicationInfo(SUPERUSER_PATH_3, 0);
			} catch (NameNotFoundException e) {
				info = null;
			}
		}

		if (info == null) {
			try {
				info = GlobalInstance.pm.getApplicationInfo(SUPERUSER_PATH_4, 0);
			} catch (NameNotFoundException e) {
				info = null;
			}
		}
		
		if (info == null) {
			try {
				info = GlobalInstance.pm.getApplicationInfo(SUPERUSER_PATH_5, 0);
			} catch (NameNotFoundException e) {
				info = null;
			}
		}

		return info != null;
	}

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

	public static boolean isWrongRoot() {
		// -rwsr-sr-x
		String suStat = runCommand("ls -l " + SU_PATH, false).result + runCommand("ls -l " + SU_PATH_X, false).result;
		if (GlobalInstance.DEBUG) {
			Log.e("suStat", suStat);
		}
		return ((!suStat.contains("-rwsr-sr-x")) && (!suStat.contains("-rwsr-xr-x")));
	}

	@SuppressWarnings("deprecation")
	public static CommandResult runCommand(String command, boolean root) {
		if (GlobalInstance.DEBUG) {
			Log.e("runRootCommand", command);
		}
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
				// if (process != null) {
				// try {
				// process.destroy();
				// } catch (Throwable th) {
				//
				// }
				// }
			} catch (Exception e) {
				ret.result = "";
				ret.error = e.getMessage();
			}
		}
		if (GlobalInstance.DEBUG) {
			Log.e("runRootCommand-Result", String.format("result:%s, error:%s", ret.result, ret.error));
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
}
