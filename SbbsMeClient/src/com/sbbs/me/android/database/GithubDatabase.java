package com.sbbs.me.android.database;

import java.io.File;

import com.sbbs.me.android.consts.PathDefine;

import android.content.ContentUris;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.net.Uri;

public class GithubDatabase {

	private SQLiteDatabase database;
	
	public static boolean isDatabaseFileExists() {
		return new File(PathDefine.DATABASE_PATH).exists();
	}

	public GithubDatabase() throws Exception {
		if (!isDatabaseFileExists()) {
			throw new Exception("No DB File");
		}
		String dbName = PathDefine.DATABASE_PATH;
		database = SQLiteDatabase.openDatabase(dbName, null,
				SQLiteDatabase.OPEN_READWRITE);
	}
	
	public Cursor doQuery(Uri uri, String[] projection, String selection,
			String[] selectionArgs, String sortOrder) {
		int actionId = -99;
		try {
			actionId = (int) ContentUris.parseId(uri);
		} catch (Exception e) {
			
		}
		if (actionId == GithubProvider.ACTION_TREELIST) {
			return database.query("SBBSGITHUB", projection, selection,
					selectionArgs, null, null, sortOrder);
		} else {
			return null;
		}
	}
	
	public void close() {
		database.close();
	}
}
