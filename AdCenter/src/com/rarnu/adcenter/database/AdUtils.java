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
}
