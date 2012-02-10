package com.snda.gyue;

import android.app.Activity;
import android.util.DisplayMetrics;

import com.snda.gyue.classes.ArticleItem;

public class GlobalInstance {
	public static float density = 0;
	public static DisplayMetrics metric = new DisplayMetrics();
	public static ArticleItem currentArticle = null;
	
	public static Activity aSplash = null;
}
