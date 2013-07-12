package com.sbbs.me.android.database;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

public class GithubProvider extends ContentProvider {

	public static final Uri CONTENT_URI = Uri
			.parse("content://com.sbbsme.github");
	
	public static final int ACTION_CLOSEDATABASE = -99;
	public static final int ACTION_NEWDATABASE = -98;
	public static final int ACTION_TREELIST = -3;
	
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
		return null;
	}

	@Override
	public boolean onCreate() {
		if (database == null) {
			try {
				database = new GithubDatabase();
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
		if (actionId == GithubProvider.ACTION_CLOSEDATABASE) {
			if (database != null) {
				database.close();
			}
			return null;
		} else if (actionId == GithubProvider.ACTION_NEWDATABASE) {
			try {
				database = new GithubDatabase();
			} catch (Exception e) {
				
			}
			return null;
		} else {
			if (database != null) {
				return database.doQuery(uri, projection, selection,
						selectionArgs, sortOrder);
			} else {
				return null;
			}
		}
	}

	@Override
	public int update(Uri uri, ContentValues values, String selection,
			String[] selectionArgs) {
		return 0;
	}

	
}
