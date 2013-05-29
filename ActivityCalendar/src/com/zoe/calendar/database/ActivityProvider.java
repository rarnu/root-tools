package com.zoe.calendar.database;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

public class ActivityProvider extends ContentProvider {

	public static final Uri CONTENT_URI = Uri
			.parse("content://com.zoe.calendar.data");

	public static final int ACTION_TABLE_ACTIVITY = -1;
	public static final int ACTION_TABLE_MOTION = -2;

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
		int actionId = 0;
		try {
			actionId = (int) ContentUris.parseId(uri);
		} catch (Exception e) {

		}
		switch (actionId) {
		case ACTION_TABLE_ACTIVITY:
			database.insert(ActivityDatabse.ACTIVITY_TABLE, values);
			break;
		case ACTION_TABLE_MOTION:
			database.insert(ActivityDatabse.MOTION_TABLE, values);
			break;
		}

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
		case ACTION_TABLE_ACTIVITY:
			c = database.query(ActivityDatabse.ACTIVITY_TABLE, selection,
					selectionArgs);
			break;
		case ACTION_TABLE_MOTION:
			c = database.query(ActivityDatabse.MOTION_TABLE, selection,
					selectionArgs);
			break;
		}

		return c;
	}

	@Override
	public int update(Uri uri, ContentValues values, String selection,
			String[] selectionArgs) {
		int actionId = 0;
		try {
			actionId = (int) ContentUris.parseId(uri);
		} catch (Exception e) {

		}
		int rowEffected = 0;
		switch (actionId) {
		case ACTION_TABLE_ACTIVITY:
			rowEffected = database.updateStatus(values, selectionArgs);
			break;
		case ACTION_TABLE_MOTION:
			rowEffected = database.updateMotion(values, selection,
					selectionArgs);
			break;
		}

		return rowEffected;

	}

}
