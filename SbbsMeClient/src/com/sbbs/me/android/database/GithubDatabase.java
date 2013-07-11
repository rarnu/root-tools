package com.sbbs.me.android.database;

import java.io.File;

import com.sbbs.me.android.consts.PathDefine;

import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.net.Uri;

public class GithubDatabase {

	private SQLiteDatabase database;
	
	public static boolean isDatabaseFileExists() {
		return new File(PathDefine.DATABASE_PATH).exists();
	}
	
	public GithubDatabase() throws Exception {
		String dbName = PathDefine.DATABASE_PATH;
		File fileDB = new File(dbName);
		if (!fileDB.exists()) {
			throw new Exception("");
		}
		database = SQLiteDatabase.openDatabase(dbName, null,
				SQLiteDatabase.OPEN_READONLY);
	}
	
	public Cursor doQuery(Uri uri, String[] projection, String selection,
			String[] selectionArgs, String sortOrder) {
		//return database.query();
		return null;
	}
	
	public void close() {
		database.close();
	}
}
