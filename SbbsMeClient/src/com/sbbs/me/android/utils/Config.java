package com.sbbs.me.android.utils;

import android.content.Context;

import com.rarnu.utils.ConfigUtils;

public class Config {

	private static final String KEY_USER_ID = "user_id";
	private static final String KEY_SINA_USER_ID = "sina_user_id";
	private static final String KEY_GOOGLE_USER_ID = "google_user_id";
	private static final String KEY_GOOGLE_USER_ACCESS_TOKEN = "google_access_token";
	private static final String KEY_GITHUB_USER_ACCESS_TOKEN = "github_access_token";
	private static final String KEY_ACCOUNT_TYPE = "account_type";
	private static final String KEY_GITHUB_USER_ID = "github_user_id";
	private static final String KEY_HEAD_PATH = "head_path";
	private static final String KEY_AVATAR_URL = "avatar_url";
	private static final String KEY_USER_NAME = "user_name";

	public static String getHeadPath(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_HEAD_PATH, "");
	}

	public static void setHeadPath(Context context, String value) {
		ConfigUtils.setStringConfig(context, KEY_HEAD_PATH, value);
	}

	public static String getUserId(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_USER_ID, "");
	}

	public static void setUserId(Context context, String value) {
		ConfigUtils.setStringConfig(context, KEY_USER_ID, value);
	}

	public static String getSinaUserId(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_SINA_USER_ID, "");
	}

	public static void setSinaUserId(Context context, String uid) {
		ConfigUtils.setStringConfig(context, KEY_SINA_USER_ID, uid);
		Config.setUserId(context, String.valueOf(uid));
	}

	public static String getGoogleUserId(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_GOOGLE_USER_ID, "");
	}

	public static String getGoogleAccessToken(Context context) {
		return ConfigUtils.getStringConfig(context,
				KEY_GOOGLE_USER_ACCESS_TOKEN, "");
	}

	public static void setGoogleUserId(Context context, String uid, String token) {
		ConfigUtils.setStringConfig(context, KEY_GOOGLE_USER_ID, uid);
		ConfigUtils.setStringConfig(context, KEY_GOOGLE_USER_ACCESS_TOKEN,
				token);
		Config.setUserId(context, String.valueOf(uid));
	}

	public static String getGithubUserId(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_GITHUB_USER_ID, "");
	}

	public static String getGithubAccessToken(Context context) {
		return ConfigUtils.getStringConfig(context,
				KEY_GITHUB_USER_ACCESS_TOKEN, "");
	}

	public static void setGithubUserId(Context context, String uid, String token) {
		ConfigUtils.setStringConfig(context, KEY_GITHUB_USER_ID, uid);
		ConfigUtils.setStringConfig(context, KEY_GITHUB_USER_ACCESS_TOKEN,
				token);
		Config.setUserId(context, uid);
	}

	/**
	 * 
	 * @param context
	 * @return account type (0:google, 1:github, 2:weibo)
	 */
	public static int getAccountType(Context context) {
		return ConfigUtils.getIntConfig(context, KEY_ACCOUNT_TYPE, -1);
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

	public static void setAvatarUrl(Context context, String value) {
		ConfigUtils.setStringConfig(context, KEY_AVATAR_URL, value);
	}

	public static String getAvatarUrl(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_AVATAR_URL, "");
	}

	public static void setUserName(Context context, String value) {
		ConfigUtils.setStringConfig(context, KEY_USER_NAME, value);
	}

	public static String getUserName(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_USER_NAME, "");
	}

	public static String getAccountString(Context context) {
		String accType = "";
		int acc = Config.getAccountType(context);
		switch (acc) {
		case 0:
			accType = "google";
			break;
		case 1:
			accType = "github";
			break;
		case 2:
			accType = "weibo";
			break;
		}
		return accType;
	}
}
