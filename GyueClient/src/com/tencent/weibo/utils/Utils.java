package com.tencent.weibo.utils;

public class Utils {
	
	/**
	 * transform int to ip string
	 * @param i
	 * @return
	 */
	public static String intToIp(int i) {   
		return ((i >> 24 ) & 0xFF ) + "." + 
	   		((i >> 16 ) & 0xFF) + "." + 
	   		((i >> 8 ) & 0xFF) + "." +
	   		( i & 0xFF);   
	}
}
