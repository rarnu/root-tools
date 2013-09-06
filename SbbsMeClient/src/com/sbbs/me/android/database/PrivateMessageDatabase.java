package com.sbbs.me.android.database;

import java.io.File;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.util.Log;

import com.sbbs.me.android.consts.PathDefine;

public class PrivateMessageDatabase {

	private SQLiteDatabase database;
	private static String databasePath = "";

	private static final String TABLE_MESSAGES = "messages";
	private static final String SQL_MESSAGES = "create table messages (id integer primary key, from_user_id text not null, from_user_name text not null, to_user_id text not null, to_user_name text not null, body text not null, created_on text not null, read integer not null)";

	public static boolean isDatabaseFileExists() {
		return new File(databasePath).exists();
	}

	public PrivateMessageDatabase(Context context) throws Exception {
		databasePath = "/data/data/" + context.getPackageName() + "/databases/";
		if (!new File(databasePath).exists()) {
			new File(databasePath).mkdirs();
		}
		databasePath += PathDefine.MESSAGE_DATA_NAME;
		Log.e("PrivateMessageDatabase", databasePath);

		if (!isDatabaseFileExists()) {
			Log.e("PrivateMessageDatabase", "Create");
			database = SQLiteDatabase.openOrCreateDatabase(databasePath, null);
			database.execSQL(SQL_MESSAGES);
		} else {
			Log.e("PrivateMessageDatabase", "Open");
			database = SQLiteDatabase.openDatabase(databasePath, null,
					SQLiteDatabase.OPEN_READWRITE);
		}
	}

	public void closeDatabase() {
		if (database != null) {
			database.close();
		}
	}

	public Cursor queryMessage(String select, String[] selectArgs) {
		Cursor c = null;
		if (database != null) {
			c = database.query(TABLE_MESSAGES, null, select, selectArgs, null,
					null, "id desc", "0,100");
		}
		return c;
	}

	public Cursor queryLastMessageId() {
		Cursor c = null;
		if (database != null) {
			c = database.query(TABLE_MESSAGES, new String[] { "id" }, null,
					null, null, null, "id desc", "0,1");
		}
		return c;
	}
	
	public Cursor queryNew(String selection, String[] args) {
		Cursor c = null;
		if (database != null) {
			c = database.query(TABLE_MESSAGES, new String[]{"read"}, selection, args, null, null, null);
		}
		return c;
	}

	public void insertMessage(ContentValues cv) {
		if (database != null) {
			if (!idExists(cv.getAsString("id"))) {
				database.insert(TABLE_MESSAGES, null, cv);
			}
		}
	}

	public void deleteMessage(String id) {
		if (database != null) {
			database.delete(TABLE_MESSAGES, "id=?", new String[] { id });
		}
	}

	public void updateMessage(ContentValues cv, String where, String[] whereArgs) {
		if (database != null) {
			database.update(TABLE_MESSAGES, cv, where, whereArgs);
		}
	}

	private boolean idExists(String id) {
		boolean ret = false;
		if (database != null) {
			Cursor c = database.query(TABLE_MESSAGES, new String[] { "id" },
					"id=?", new String[] { id }, null, null, null);
			if (c != null) {
				c.moveToFirst();
				while (!c.isAfterLast()) {
					ret = true;
					c.moveToNext();
				}
				c.close();
			}
		}
		return ret;
	}

}
