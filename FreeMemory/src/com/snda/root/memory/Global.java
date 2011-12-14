package com.snda.root.memory;

import com.snda.root.memory.utils.ProcessInfo;

import android.content.pm.PackageManager;

public class Global {

	public static PackageManager pm = null;
	public static int myPid = 0;
	
	public static ProcessInfo currentProcInfo = null;
}
