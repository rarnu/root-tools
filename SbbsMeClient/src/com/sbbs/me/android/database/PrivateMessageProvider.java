package com.sbbs.me.android.database;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

public class PrivateMessageProvider extends ContentProvider {

	public static final Uri CONTENT_URI = Uri
			.parse("content://com.sbbsme.message");

	public static final int ACTION_MESSAGE = 1;
	public static final int ACTION_LAST_MESSAGE_ID = 2;
	public static final int ACTION_QUERY_NEW = 3;

	private PrivateMessageDatabase database = null;

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
		int actionId = -99;
		try {
			actionId = (int) ContentUris.parseId(uri);
		} catch (Exception e) {

		}
		if (database != null) {
			switch (actionId) {
			case ACTION_MESSAGE:
				database.insertMessage(values);
				break;
			}
		}
		return null;
	}

	@Override
	public boolean onCreate() {
		try {
			database = new PrivateMessageDatabase(getContext());
		} catch (Exception e) {

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
		Cursor c = null;
		if (database != null) {
			switch (actionId) {
			case ACTION_MESSAGE:
				c = database.queryMessage(selection, selectionArgs);
				break;
			case ACTION_LAST_MESSAGE_ID:
				c = database.queryLastMessageId();
				break;
			case ACTION_QUERY_NEW:
				c = database.queryNew(selection, selectionArgs);
				break;
			}
		}
		return c;
	}

	@Override
	public int update(Uri uri, ContentValues values, String selection,
			String[] selectionArgs) {
		int actionId = -99;
		try {
			actionId = (int) ContentUris.parseId(uri);
		} catch (Exception e) {

		}
		if (database != null) {
			switch (actionId) {
			case ACTION_MESSAGE:
				database.updateMessage(values, selection, selectionArgs);
				break;
			}
		}
		return 0;
	}

}
