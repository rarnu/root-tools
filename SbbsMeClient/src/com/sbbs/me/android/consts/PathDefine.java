package com.sbbs.me.android.consts;

import java.io.File;

import android.os.Environment;

public class PathDefine {

	public static final String ROOT_PATH = Environment
			.getExternalStorageDirectory().getPath() + "/.sbbsme/";
	public static final String DATA_NAME = "sbbsme.db";
	public static final String ARTICLE_LIST = "article.list";
	public static final String TAG_LIST = "tag.list";
	public static final String GALLERY_LIST = "gallery.list";
	public static final String DATABASE_PATH = ROOT_PATH + DATA_NAME;
	public static final String CACHE_ARTICLE_LIST_PATH = ROOT_PATH
			+ ARTICLE_LIST;
	public static final String CACHE_TAG_LIST_PATH = ROOT_PATH + TAG_LIST;
	public static final String CACHE_GALLERY_LIST = ROOT_PATH + GALLERY_LIST;

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
