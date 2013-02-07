package com.rarnu.installer.func;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import android.content.Context;
import android.os.Environment;

import com.rarnu.tools.root.utils.root.CommandResult;
import com.rarnu.tools.root.utils.root.RootUtils;

public class GameCenterInstaller {

	public static final int EC_SUCC = 0;
	public static final int EC_NO_UINPUT = 1;
	public static final int EC_EXTRACT_ASSETS = 2;
	public static final int EC_INSTALL_SYSTEM_FILES = 3;
	public static final int EC_START_SERVICE = 4;
	public static final int EC_REMOUNT = 5;

	private static String tmpPath = Environment.getExternalStorageDirectory()
			+ "/.gci/";

	public static int install(Context context) {
		RootUtils.mountRW();

		if (!uinputExists()) {
			return EC_NO_UINPUT;
		}
		if (!extractAssets(context)) {
			return EC_EXTRACT_ASSETS;
		}
		if (!installSystemFiles()) {
			return EC_INSTALL_SYSTEM_FILES;
		}
		if (!startXcService()) {
			return EC_START_SERVICE;
		}
		return EC_SUCC;
	}

	private static boolean uinputExists() {
		return new File("/dev/uinput").exists();
	}

	private static boolean extractAssets(Context context) {
		int succ = 0;
		if (saveAssetsToFile(context, "sensors.default.so"))
			succ++;
		if (saveAssetsToFile(context, "set_acc"))
			succ++;
		if (saveAssetsToFile(context, "set_gyr"))
			succ++;
		if (saveAssetsToFile(context, "xc_cmd"))
			succ++;
		if (saveAssetsToFile(context, "xcmidware"))
			succ++;
		if (saveAssetsToFile(context, "xcuts.idc"))
			succ++;
		if (saveAssetsToFile(context, "XCGameCenter.apk"))
			succ++;
		return succ == 7;
	}

	private static boolean installSystemFiles() {
		String binPath = "/system/bin/";
		String libPath = "/system/lib/hw";
		String appPath = "/system/app/";
		String idcPath = "/system/usr/idc/";
		int succ = 0;
		if (copyFileToSystem("sensors.default.so", libPath, false))
			succ++;
		if (copyFileToSystem("set_acc", binPath, true))
			succ++;
		if (copyFileToSystem("set_gyr", binPath, true))
			succ++;
		if (copyFileToSystem("xc_cmd", binPath, true))
			succ++;
		if (copyFileToSystem("xcmidware", binPath, true))
			succ++;
		if (copyFileToSystem("xcuts.idc", idcPath, false))
			succ++;
		if (copyFileToSystem("XCGameCenter.apk", appPath, false))
			succ++;
		if (deleteSensorFiles())
			succ++;
		return succ == 8;
	}

	private static boolean saveAssetsToFile(Context context, String assets) {

		File fTemp = new File(tmpPath);
		if (!fTemp.exists()) {
			fTemp.mkdirs();
		}
		try {
			byte[] buffer = new byte[8192];

			File dest = new File(tmpPath + assets);

			if (dest.exists()) {
				dest.delete();
			}

			InputStream is = context.getAssets().open(assets);
			OutputStream fos = new BufferedOutputStream(new FileOutputStream(
					dest));
			int n;
			while ((n = is.read(buffer, 0, buffer.length)) != -1)
				fos.write(buffer, 0, n);

			is.close();
			fos.close();
			return true;
		} catch (Exception ex) {
			return false;
		}
	}

	private static boolean copyFileToSystem(String path, String sysPath,
			boolean setPermission) {
		CommandResult ret = RootUtils.runCommand(
				String.format("cat %s%s > %s%s", tmpPath, path, sysPath, path),
				true);
		if (setPermission) {
			ret = RootUtils.runCommand(
					String.format("chmod 777 %s%s", sysPath, path), true);
		}
		return ret.error.equals("");
	}

	private static boolean deleteSensorFiles() {
		CommandResult ret = RootUtils.runCommand("rm /system/lib/hw/sensors*",
				true);
		return ret.error.equals("");
	}

	public static boolean startXcService() {
		CommandResult ret = RootUtils.runCommand("xcmidware &", true);
		return ret.error.equals("");
	}
	
	public static boolean XcServiceExists() {
		CommandResult ret = RootUtils.runCommand("ps | grep xcmidware", true);
		return ret.result.contains("xcmidware");
	}
}
