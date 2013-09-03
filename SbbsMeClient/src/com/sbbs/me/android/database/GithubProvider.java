package com.sbbs.me.android.database;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

public class GithubProvider extends ContentProvider {

	public static final Uri CONTENT_URI = Uri
			.parse("content://com.sbbsme.github");

	public static final int ACTION_GITHUB_CACHE = 1;

	private GithubDatabase database = null;

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
			case ACTION_GITHUB_CACHE:
				database.insertOrUpdateGithubCache(values);
				break;
			}
		}
		return null;
	}

	@Override
	public boolean onCreate() {
		try {
			database = new GithubDatabase(getContext());
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
			case ACTION_GITHUB_CACHE:
				c = database.queryGithubCache(selection, selectionArgs);
				break;
			}
		}
		return c;
	}

	@Override
	public int update(Uri uri, ContentValues values, String selection,
			String[] selectionArgs) {
		return 0;
	}

}
