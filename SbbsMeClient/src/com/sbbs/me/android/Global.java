package com.sbbs.me.android;

import java.util.List;

import com.sbbs.me.android.api.SbbsMeBlock;

public class Global {

	public static List<SbbsMeBlock> listArticle = null;
	
	public static void releaseAll() {
		listArticle = null;
	}
}
