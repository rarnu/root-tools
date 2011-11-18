package com.snda.root.sapp.manager;

import android.content.pm.PackageManager;

import com.snda.root.sapp.manager.adapter.AppInfo;

public class GlobalInstance {
	public static PackageManager pm = null;
	public static AppInfo currentApp = null;
	
	public static boolean allowDeleteLevel0 = false;
	public static boolean colorLevel = true;
}
