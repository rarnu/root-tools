package com.rarnu.zoe.loving;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

public class Config {

	private static final String ACCOUNT = "user_account";
	private static final String TOKEN = "user_token";
	private static final String NICKNAME = "user_nickname";
	private static final String ID = "user_id";
	private static final String TEMP_ARTICLE = "temp_article";

	private static SharedPreferences sp = null;
	private static void initSharedPreference(Context context) {
		if (sp == null) {
			sp = PreferenceManager.getDefaultSharedPreferences(context);
		}
	}
	public static String getAccount(Context context) {
		initSharedPreference(context);
		return sp.getString(ACCOUNT, "");
	}
	
	public static void setAccount(Context context, String account) {
		initSharedPreference(context);
		sp.edit().putString(ACCOUNT, account).commit();
	}
	
	public static String getToken(Context context) {
		initSharedPreference(context);
		return sp.getString(TOKEN, "");
	}
	
	public static void setToken(Context context, String token) {
		initSharedPreference(context);
		sp.edit().putString(TOKEN, token).commit();
	}
	
	public static String getNickname(Context context) {
		initSharedPreference(context);
		return sp.getString(NICKNAME, "");
	}
	
	public static void setNickname(Context context, String nickname) {
		initSharedPreference(context);
		sp.edit().putString(NICKNAME, nickname).commit();
	}

	public static int getId(Context context) {
		initSharedPreference(context);
		return sp.getInt(ID, 0);
	}
	
	public static void setId(Context context, int value) {
		initSharedPreference(context);
		sp.edit().putInt(ID, value).commit();
	}
	
	public static String getTempArticle(Context context) {
		initSharedPreference(context);
		return sp.getString(TEMP_ARTICLE, "");
	}
	
	public static void setTempArticle(Context context, String value) {
		initSharedPreference(context);
		sp.edit().putString(TEMP_ARTICLE, value).commit();
	}
}
