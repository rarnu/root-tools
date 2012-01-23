package com.snda.root;

import android.content.pm.PackageManager;

public class GlobalInstance {

	public static PackageManager pm;
	
	public static void SetPackageManager(PackageManager _pm) {
		pm = _pm;
	}
}
