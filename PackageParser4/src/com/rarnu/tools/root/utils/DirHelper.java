package com.rarnu.tools.root.utils;

import java.io.File;

import android.os.Environment;

public class DirHelper {

	public static final String ROOT_DIR = Environment
			.getExternalStorageDirectory().getPath() + "/.root_tools/";
	public static final String SYSAPP_DIR = ROOT_DIR + "sysapp/";
	public static final String ENABLEAPP_DIR = ROOT_DIR + "enableapp/";
	public static final String ENABLEAPP_DIR_SYSTEM = ENABLEAPP_DIR + "system/";
	public static final String ENABLEAPP_DIR_DATA = ENABLEAPP_DIR + "data/";
	public static final String DATAAPP_DIR = ROOT_DIR + "data/";
	public static final String HOSTS_DIR = ROOT_DIR + "hosts/";
	public static final String BUSYBOX_DIR = ROOT_DIR + "busybox/";
	public static final String MEM_DIR = ROOT_DIR + "mem/";
	public static final String TEMP_DIR = ROOT_DIR + "tmp/";
	public static final String ERROR_DIR = ROOT_DIR + "error/";
	public static final String ICON_DIR = ROOT_DIR + "icon/";
	public static final String FORCE_UPDATE_DIR = ROOT_DIR + "force_update/";

	public static void makeDir() {
		makeDir(ROOT_DIR);
		makeDir(SYSAPP_DIR);
		makeDir(ENABLEAPP_DIR);
		makeDir(ENABLEAPP_DIR_SYSTEM);
		makeDir(ENABLEAPP_DIR_DATA);
		makeDir(DATAAPP_DIR);
		makeDir(HOSTS_DIR);
		makeDir(MEM_DIR);
		makeDir(TEMP_DIR);
		makeDir(ERROR_DIR);
		makeDir(ICON_DIR);
		makeDir(FORCE_UPDATE_DIR);
	}

	private static void makeDir(String path) {
		File fDir = new File(path);
		if (!fDir.exists()) {
			fDir.mkdirs();
		}
	}
}
