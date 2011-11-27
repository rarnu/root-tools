package com.snda.root.hosts;

import java.util.List;
import java.util.Map;

public class GlobalInstance {
	
	public static String app_dir = "/sdcard/.nshosts/";
	public static String sys_dir = "/system/etc/";
	public static String host_filename = "hosts";
	public static String domain_filename = "domain";
	public static String common_site_url = "http://rarnu.7thgen.info/snda/roottool/common_site";

	public static List<Map<String, String >> passedHosts = null;
	public static List<Map<String, String >> testHosts = null;
	public static List<Map<String, String >> deprecatedHosts = null;
	
	public static String hostsText = "";
	
	public static String nameServer = "";
	public static boolean autoSelect = true;
}
