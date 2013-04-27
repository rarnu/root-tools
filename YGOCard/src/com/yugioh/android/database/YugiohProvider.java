package com.yugioh.android.database;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

public class YugiohProvider extends ContentProvider {

	public static final Uri CONTENT_URI = Uri
			.parse("content://com.yugioh.card");

	public static final int ACTIONID_CLOSEDATABASE = -99;
	public static final int ACTIONID_NEWDATABASE = -98;
	public static final int ACTIONID_CARDCOUNT = -4;
	public static final int ACTIONID_EFFECTLIST = -3;
	public static final int ACTIONID_TOP100 = -2;
	public static final int ACTIONID_SEARCH = -1;

	private YugiohDatabase database = null;

	@Override
	public int delete(Uri uri, String selection, String[] selectionArgs) {
		return 0;
	}

	@Override
	public String getType(Uri uri) {
		return null;
	}

	@Override
	public Uri insert(Uri uri, ContentValues values) {
		return null;
	}

	@Override
	public boolean onCreate() {
		if (database == null) {
			try {
				database = new YugiohDatabase(getContext());
			} catch (Exception e) {
			}
		}
		return (database != null);
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection,
			String[] selectionArgs, String sortOrder) {

		int actionId = -99;
		try {
			actionId = (int) ContentUris.parseId(uri);
		} catch (Exception e) {

		}
		if (actionId == YugiohProvider.ACTIONID_CLOSEDATABASE) {
			database.close();
			return null;
		} else if (actionId == YugiohProvider.ACTIONID_NEWDATABASE) {
			try {
				database = new YugiohDatabase(getContext());
			} catch (Exception e) {
				
			}
			return null;
		} else {

			return database.doQuery(uri, projection, selection, selectionArgs,
					sortOrder);
		}
	}

	@Override
	public int update(Uri uri, ContentValues values, String selection,
			String[] selectionArgs) {
		return 0;
	}

}
