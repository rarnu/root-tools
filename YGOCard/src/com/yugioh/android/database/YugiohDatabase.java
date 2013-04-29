package com.yugioh.android.database;

import java.io.File;

import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.net.Uri;

import com.yugioh.android.R;
import com.yugioh.android.define.PathDefine;

public class YugiohDatabase {

	private SQLiteDatabase database;

	public static boolean isDatabaseFileExists() {
		return new File(PathDefine.DATABASE_PATH).exists();
	}

	public YugiohDatabase(Context context) throws Exception {
		// this.context = context;
		String dbName = PathDefine.DATABASE_PATH;
		File fDb = new File(dbName);
		if (!fDb.exists()) {
			throw new Exception(context.getResources().getString(
					R.string.error_no_database));
		}

		database = SQLiteDatabase.openDatabase(dbName, null,
				SQLiteDatabase.OPEN_READONLY);
	}

	public Cursor doQuery(Uri uri, String[] projection, String selection,
			String[] selectionArgs, String sortOrder) {
		int actionId = -99;
		try {
			actionId = (int) ContentUris.parseId(uri);
		} catch (Exception e) {

		}
		if (actionId == YugiohProvider.ACTIONID_CARDCOUNT) {
			return database
					.rawQuery(
							"select CardID from YGODATA order by CardID desc limit 0,1",
							null);
		} else if (actionId == YugiohProvider.ACTIONID_EFFECTLIST) {
			return database.rawQuery("select * from YGOEFFECT", null);
		} else if (actionId == YugiohProvider.ACTIONID_TOP100) {
			return database
					.rawQuery(
							"select _id, SCCardName, SCCardType from YGODATA order by _id desc limit 0,100 ",
							null);
		} else if (actionId == YugiohProvider.ACTIONID_SEARCH) {
			return database.query("YGODATA", projection, selection,
					selectionArgs, null, null, sortOrder);
		} else if (actionId >= 0) {
			return database.rawQuery("select * from YGODATA where CardID=?",
					new String[] { String.valueOf(actionId) });
		} else {
			return null;
		}

	}

	public void close() {
		database.close();
	}

}
