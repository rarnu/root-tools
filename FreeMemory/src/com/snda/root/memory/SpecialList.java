package com.snda.root.memory;

import java.util.ArrayList;
import java.util.List;

public class SpecialList {

	private static List<String> lstExclude = new ArrayList<String>();
	private static List<String> lstForceKill = new ArrayList<String>();
	
	static {
		// self
		lstExclude.add("com.snda.root.memory");
		
		// system
		lstExclude.add("com.android.systemui");
		lstExclude.add("com.android.phone");
		
	}
	
	public static boolean inExcludeList(String ns) {
		
		return lstExclude.indexOf(ns) != -1;
	}
	
	public static boolean inKillList(String ns) {
		return lstForceKill.indexOf(ns) != -1;
	}
}
