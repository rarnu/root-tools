package com.rarnu.adcenter.database;

import java.util.ArrayList;
import java.util.List;

import android.content.ContentResolver;
import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;

import com.rarnu.adcenter.classes.AdItem;
import com.rarnu.adcenter.classes.UserItem;

public class AdUtils {

	public static void saveAds(Context context, List<AdItem> list) {
		ContentResolver cr = context.getContentResolver();
		Uri u = ContentUris.withAppendedId(AdProvider.CONTENT_URI,
				AdProvider.ACTION_SAVE_ADS);
		if (list != null) {
			for (int i = 0; i < list.size(); i++) {
				ContentValues cv = new ContentValues();
				cv.put("id", list.get(i).id);
				cv.put("quest_answered", 0);
				cr.insert(u, cv);
			}
		}
	}

	public static boolean getAdQuested(Context context, int adId) {
		Cursor c = context.getContentResolver().query(
				ContentUris.withAppendedId(AdProvider.CONTENT_URI,
						AdProvider.ACTION_QUERY_AD_QUESTED), null, "id=?",
				new String[] { String.valueOf(adId) }, null);
		boolean ret = false;
		if (c != null) {
			c.moveToFirst();
			while (!c.isAfterLast()) {
				ret = c.getInt(c.getColumnIndex("quest_answered")) != 0;
				c.moveToNext();
			}
			c.close();
		}
		return ret;
	}

	public static void setAdQuested(Context context, int adId) {
		ContentValues cv = new ContentValues();
		cv.put("quest_answered", 1);
		context.getContentResolver().update(
				ContentUris.withAppendedId(AdProvider.CONTENT_URI,
						AdProvider.ACTION_SET_AD_QUESTED), cv, "id=?",
				new String[] { String.valueOf(adId) });
	}

	public static List<Boolean> getListQuestedState(Context context,
			List<AdItem> list) {
		List<Boolean> ret = new ArrayList<Boolean>();
		for (int i = 0; i < list.size(); i++) {
			ret.add(getAdQuested(context, list.get(i).id));
		}
		return ret;
	}

	public static void login(Context context, UserItem user) {
		ContentValues cv = new ContentValues();
		cv.put("id", user.id);
		cv.put("name", user.name);
		cv.put("account", user.account);
		cv.put("cash", user.cash);
		context.getContentResolver().insert(
				ContentUris.withAppendedId(AdProvider.CONTENT_URI,
						AdProvider.ACTION_LOGIN), cv);
	}

	public static void logout(Context context, UserItem user) {
		context.getContentResolver().delete(
				ContentUris.withAppendedId(AdProvider.CONTENT_URI,
						AdProvider.ACTION_LOGOUT), "id=?",
				new String[] { String.valueOf(user.id) });
	}

	public static UserItem queryUser(Context context) {
		UserItem user = null;
		Cursor c = context.getContentResolver().query(
				ContentUris.withAppendedId(AdProvider.CONTENT_URI,
						AdProvider.ACTION_QUERY_USER), null, null, null, null);
		if (c != null) {
			c.moveToFirst();
			while (!c.isAfterLast()) {
				user = new UserItem();
				user.id = c.getInt(c.getColumnIndex("id"));
				user.account = c.getString(c.getColumnIndex("account"));
				user.name = c.getString(c.getColumnIndex("name"));
				user.cash = c.getInt(c.getColumnIndex("cash"));
				break;
			}
			c.close();
		}
		return user;
	}

	public static void updateCash(Context context, int userId, int cash) {
		ContentValues cv = new ContentValues();
		cv.put("cash", cash);
		context.getContentResolver().update(
				ContentUris.withAppendedId(AdProvider.CONTENT_URI,
						AdProvider.ACTION_UPDATE_CASH), cv, "id=?",
				new String[] { String.valueOf(userId) });
	}
}
