package com.yugioh.android.define;

import java.io.File;

public class PathDefine {

	public static final String ROOT_PATH = "/sdcard/.yugioh/";
	public static final String APK_NAME = "YuGiOhCard.apk";
	public static final String DATA_ZIP = "yugioh.zip";
	public static final String DATA_NAME = "yugioh.db";
	public static final String DATABASE_PATH = ROOT_PATH + DATA_NAME;
	public static final String PICTURE_PATH = ROOT_PATH + "images/";
	public static final String DOWNLOAD_PATH = ROOT_PATH + "downloads/";
	public static final String RECOMMAND_PATH = ROOT_PATH + "recommand/";
	
	static {
		mkdir(ROOT_PATH);
		mkdir(PICTURE_PATH);
		mkdir(DOWNLOAD_PATH);
		mkdir(RECOMMAND_PATH);
	}
	
	private static void mkdir(String path) {
		File fPath = new File(path);
		if (!fPath.exists()) {
			fPath.mkdirs();
		}
	}
	
}
