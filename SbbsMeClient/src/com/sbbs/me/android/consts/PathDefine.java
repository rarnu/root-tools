package com.sbbs.me.android.consts;

import java.io.File;

public class PathDefine {

	public static final String ROOT_PATH = "/sdcard/.sbbsme/";
	public static final String APK_NAME = "SbbsMeClient.apk";
	public static final String DATA_ZIP = "sbbsme.zip";
	public static final String DATA_NAME = "sbbsme.db";
	public static final String DATABASE_PATH = ROOT_PATH + DATA_NAME;
	
	static {
		mkdir(ROOT_PATH);
	}
	
	private static void mkdir(String path) {
		File fPath = new File(path);
		if (!fPath.exists()) {
			fPath.mkdirs();
		}
	}
}
