package com.rarnu.findaround.common;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

public class Config {

	public static List<String> getHistoryList(Context context) {
		String path = "/data/data/" + context.getPackageName() + "/history";

		try {
			List<String> ret = FileUtils.readFile(new File(path));
			for (int i = ret.size() - 1; i >= 0; i--) {
				if (ret.get(i).trim().equals("")) {
					ret.remove(i);
				}
			}
			return ret;
		} catch (Exception e) {
			return new ArrayList<String>();
		}
	}

	public static void saveHistoryList(Context context, List<String> list) {
		String key = "";
		if (list != null && list.size() > 0) {
			for (int i = 0; i < list.size(); i++) {
				if (!list.get(i).trim().equals("")) {
					key += list.get(i) + "\n";
				}
			}
		}
		setHistory(context, key);
	}

	public static void setHistory(Context context, String text) {
		String path = "/data/data/" + context.getPackageName() + "/history";

		try {
			FileUtils.rewriteFile(new File(path), text);
		} catch (IOException e) {
		}
	}

	public static List<String> getKeywordsList(Context context) {

		String path = "/data/data/" + context.getPackageName() + "/keywords";
		try {
			List<String> ret = FileUtils.readFile(new File(path));
			for (int i = ret.size() - 1; i >= 0; i--) {
				if (ret.get(i).trim().equals("")) {
					ret.remove(i);
				}
			}
			return ret;
		} catch (IOException e) {
			return new ArrayList<String>();
		}
	}

	public static void saveKeywordList(Context context, List<String> list) {
		String key = "";
		if (list != null && list.size() > 0) {
			for (int i = 0; i < list.size(); i++) {
				if (!list.get(i).trim().equals("")) {
					key += list.get(i) + "\n";
				}
			}
		}
		setKeywords(context, key);
	}

	public static String getKeywordsText(Context context) {
		List<String> list = getKeywordsList(context);
		String ret = "";
		if (list != null && list.size() != 0) {
			for (String s : list) {
				ret += s + "\n";
			}
		}
		return ret;
	}

	public static void setKeywords(Context context, String text) {
		String path = "/data/data/" + context.getPackageName() + "/keywords";

		try {
			FileUtils.rewriteFile(new File(path), text);
		} catch (IOException e) {
		}
	}

	public static int getDist(Context context) {
		return getSharedPreferences(context).getInt(KEY_DIST, 1000);
	}

	public static int getResultCount(Context context) {
		return getSharedPreferences(context).getInt(KEY_RESULT_COUNT, 10);
	}

	public static void setResultCount(Context context, int value) {
		getSharedPreferences(context).edit().putInt(KEY_RESULT_COUNT, value)
				.commit();
	}

	public static int getMethod(Context context) {
		return getSharedPreferences(context).getInt(KEY_METHOD, 2);
	}

	public static void setDist(Context context, int value) {
		getSharedPreferences(context).edit().putInt(KEY_DIST, value).commit();
	}

	public static void setMethod(Context context, int value) {
		getSharedPreferences(context).edit().putInt(KEY_METHOD, value).commit();
	}

	private final static String KEY_DIST = "key_dist";
	private final static String KEY_METHOD = "key_method";
	private final static String KEY_RESULT_COUNT = "key_result_count";

	private static SharedPreferences sp = null;

	private static SharedPreferences getSharedPreferences(Context context) {
		if (sp == null) {
			sp = PreferenceManager.getDefaultSharedPreferences(context);
		}
		return sp;
	}
}
