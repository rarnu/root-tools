package com.snda.root.memory.utils;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.NameNotFoundException;

import com.snda.root.memory.Global;

public class ProcessInfo {

	public String USER;
	public int PID;
	public int PPID;
	public int VSIZE;
	public int RSS;
	public String WCHAN;
	public String PC;
	public String STAT;
	public String NAME;
	public ApplicationInfo appInfo = null;

	public static ProcessInfo stringToProcessInfo(String str) {
		str = str.replaceAll("\\s+", " ");
		String[] ss = str.split(" ");
		ProcessInfo info = new ProcessInfo();
		info.USER = ss[0];
		info.PID = Integer.parseInt(ss[1]);
		info.PPID = Integer.parseInt(ss[2]);
		info.VSIZE = Integer.parseInt(ss[3]) / 1024;
		info.RSS = Integer.parseInt(ss[4]) / 1024;
		info.WCHAN = ss[5];
		info.PC = ss[6];
		info.STAT = ss[7];
		info.NAME = ss[8];

		try {
			info.appInfo = findApplicationByNamespace(ss[8]);
		} catch (NameNotFoundException e) {
		}
		
		if (info.NAME.equals("com.snda.root.memory")) {
			Global.myPid = info.PID;
		}

		return info;
	}

	private static ApplicationInfo findApplicationByNamespace(String ns)
			throws NameNotFoundException {
		return Global.pm.getApplicationInfo(ns, 0);
	}
}
