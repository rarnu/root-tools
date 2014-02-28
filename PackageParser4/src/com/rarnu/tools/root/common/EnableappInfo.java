package com.rarnu.tools.root.common;

import android.content.pm.ApplicationInfo;

public class EnableappInfo {
	public ApplicationInfo info;
	public String log;
	
	// 0: succ | 1: exists | 2: fail
	public int logId;
	public Boolean enabled;
	
	// 0: system | 1:data | 2: internal | 3: private
	public int type;
	public String filePath;
}
