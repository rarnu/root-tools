package com.sbbs.me.android.utils;

import android.content.Context;

import com.rarnu.utils.ConfigUtils;

public class Config {

	private static final String KEY_SINA_USER_ID = "sina_user_id";
	private static final String KEY_ACCOUNT_TYPE = "account_type";

	public static long getSinaUserId(Context context) {
		return ConfigUtils.getLongConfig(context, KEY_SINA_USER_ID, 0L);
	}

	public static void setSinaUserId(Context context, long uid) {
		ConfigUtils.setLongConfig(context, KEY_SINA_USER_ID, uid);
	}

	/**
	 * 
	 * @param context
	 * @return account type (0:google, 1:github, 2:weibo)
	 */
	public static int getAccountType(Context context) {
		return ConfigUtils.getIntConfig(context, KEY_ACCOUNT_TYPE, 0);
	}

	/**
	 * 
	 * @param context
	 * @param type
	 *            (0:google, 1:github, 2:weibo)
	 */
	public static void setAccountType(Context context, int type) {
		ConfigUtils.setIntConfig(context, KEY_ACCOUNT_TYPE, type);
	}
}
