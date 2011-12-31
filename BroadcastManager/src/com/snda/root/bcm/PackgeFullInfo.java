package com.snda.root.bcm;

import android.content.pm.PackageParser;
import android.graphics.drawable.Drawable;

public class PackgeFullInfo {

	public PackageParser.Package pack;
	public Drawable icon;
	public String label;
	
	public int receiverCount;
	public int disabledReceiver;
	public int enabledReceiver;
	
	public int permissionCount;
	public boolean isSytemApp;
}
