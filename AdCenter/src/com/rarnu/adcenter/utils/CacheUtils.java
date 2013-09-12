package com.rarnu.adcenter.utils;

import java.io.File;

import android.os.Environment;

public class CacheUtils {

	public static String CACHE_PATH = Environment.getExternalStorageDirectory()
			.getAbsolutePath() + "/.adcenter/";
	static {
		File fCache = new File(CACHE_PATH);
		if (!fCache.exists()) {
			fCache.mkdirs();
		}
	}
}
