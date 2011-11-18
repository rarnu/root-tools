package com.snda.root.sapp.manager.utils;

import java.io.File;

import android.os.Environment;

public class SDCardUtils {

	public static final String dir = "/sdcard/.sapp";
	
	public static boolean isSdcardExists() {
		return (Environment.getExternalStorageState()
				.equals(Environment.MEDIA_MOUNTED));
	}
	
	public static void makeAppDir() {
		File f = new File(dir);
		if (!f.exists()) {
			f.mkdirs();
		}
	}
	
}
