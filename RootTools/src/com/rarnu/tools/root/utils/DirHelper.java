package com.rarnu.tools.root.utils;

import java.io.File;

import android.os.Environment;

public class DirHelper {

	private static final String ROOT_DIR = "/sdcard/.root_tools/";
	public static final String SYSAPP_DIR = ROOT_DIR + "sysapp/";
	public static final String DATAAPP_DIR = ROOT_DIR + "data/";
	public static final String HOSTS_DIR = ROOT_DIR + "hosts/";
	public static final String BUSYBOX_DIR = ROOT_DIR + "busybox/";
	public static final String MEM_DIR = ROOT_DIR + "mem/";
	public static final String TEMP_DIR = ROOT_DIR + "tmp/";
	public static final String ERROR_DIR = ROOT_DIR + "error/";
	
	public static void makeDir() {
		makeDir(ROOT_DIR);
		makeDir(SYSAPP_DIR);
		makeDir(DATAAPP_DIR);
		makeDir(HOSTS_DIR);
		makeDir(MEM_DIR);
		makeDir(TEMP_DIR);
		makeDir(ERROR_DIR);
	}
	
	public static boolean isSDCardExists() {
		return (Environment.getExternalStorageState()
				.equals(Environment.MEDIA_MOUNTED));
	}
	
	private static void makeDir(String path) {
		File fDir = new File(path);
		if (!fDir.exists()) {
			fDir.mkdirs();
		}
	}
}
