package com.snda.root.bcm;

import android.content.pm.PackageParser;
import android.graphics.drawable.Drawable;

public class PackageFullInfo {

	public PackageParser.Package pack;
	public Drawable icon;
	public String label;
	
	public int receiverCount;
	public int disabledReceiver;
	public int enabledReceiver;
	
	public int serviceCount;
	public int disabledService;
	public int enabledService;
	
	public int permissionCount;
	public boolean isSytemApp;
}
