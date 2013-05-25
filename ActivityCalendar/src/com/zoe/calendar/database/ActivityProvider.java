package com.zoe.calendar.database;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

public class ActivityProvider extends ContentProvider {

	public static final Uri CONTENT_URI = Uri
			.parse("content://com.zoe.calendar.data");

	public static final int ACTIONID_CLOSEDATABASE = -99;
	public static final int ACTIONID_QUERY = -1;

	ActivityDatabse database = null;

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
		database = new ActivityDatabse(getContext());
		return true;
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection,
			String[] selectionArgs, String sortOrder) {
		int actionId = 0;
		try {
			actionId = (int) ContentUris.parseId(uri);
		} catch (Exception e) {

		}

		Cursor c = null;

		switch (actionId) {
		case ACTIONID_CLOSEDATABASE:
			database.releaseDatabase();
			break;
		case ACTIONID_QUERY:
			c = database.query(selection, selectionArgs);
			break;
		}

		return c;
	}

	@Override
	public int update(Uri uri, ContentValues values, String selection,
			String[] selectionArgs) {
		return database.updateStatus(values, selectionArgs);
	}

}
