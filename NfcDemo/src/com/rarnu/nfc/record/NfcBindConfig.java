package com.rarnu.nfc.record;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

public class NfcBindConfig {

	private static SharedPreferences sp = null;

	private static void initSharedPreference(Context context) {
		if (sp == null) {
			sp = PreferenceManager.getDefaultSharedPreferences(context);
		}
	}
	
	public static void saveBindConfig(Context context, String tag, String component) {
		initSharedPreference(context);
		sp.edit().putString(tag, component).commit();
	}
	
	public static String loadBindConfig(Context context, String tag) {
		initSharedPreference(context);
		return sp.getString(tag, "");
	}
}
