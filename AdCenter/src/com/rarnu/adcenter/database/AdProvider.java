package com.rarnu.adcenter.database;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

public class AdProvider extends ContentProvider {

	public static final Uri CONTENT_URI = Uri
			.parse("content://com.rarnu.adcenter");

	AdDatabase database = null;

	public static final int ACTION_QUERY_AD_QUESTED = 1;
	public static final int ACTION_SET_AD_QUESTED = 2;
	public static final int ACTION_SAVE_ADS = 3;

	@Override
	public boolean onCreate() {
		try {
			database = new AdDatabase(getContext());
		} catch (Exception e) {
		}
		return database != null;
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection,
			String[] selectionArgs, String sortOrder) {
		int action = (int) ContentUris.parseId(uri);
		Cursor c = null;
		if (database != null) {
			switch (action) {
			case ACTION_QUERY_AD_QUESTED:
				c = database.queryAdQuested(selection, selectionArgs);
				break;
			}
		}
		return c;
	}

	@Override
	public String getType(Uri uri) {
		return null;
	}

	@Override
	public Uri insert(Uri uri, ContentValues values) {
		int action = (int) ContentUris.parseId(uri);
		if (database != null) {
			switch (action) {
			case ACTION_SAVE_ADS:
				database.saveAd(values);
				break;
			}
		}
		return null;
	}

	@Override
	public int delete(Uri uri, String selection, String[] selectionArgs) {
		return 0;
	}

	@Override
	public int update(Uri uri, ContentValues values, String selection,
			String[] selectionArgs) {
		int action = (int) ContentUris.parseId(uri);

		if (database != null) {
			switch (action) {
			case ACTION_SET_AD_QUESTED:
				database.setAdQuested(values, selection, selectionArgs);
				break;
			}
		}
		return 0;
	}

}
