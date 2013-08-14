package com.sbbs.me.android.database;

import java.io.File;

import android.content.ContentValues;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.util.Log;

import com.sbbs.me.android.consts.PathDefine;

public class PrivateMessageDatabase {

	private SQLiteDatabase database;

	private static final String TABLE_MESSAGES = "messages";
	private static final String SQL_MESSAGES = "create table messages (_id integer primary key autoincrement, id text not null, from_user_id text not null, from_user_name text not null, to_user_id text not null, to_user_name text not null, format text not null, body text not null, created_on text not null, read integer not null)";

	public static boolean isDatabaseFileExists() {
		return new File(PathDefine.MESSAGE_DATABASE_PATH).exists();
	}

	public PrivateMessageDatabase() throws Exception {
		if (!isDatabaseFileExists()) {
			database = SQLiteDatabase.openOrCreateDatabase(
					PathDefine.MESSAGE_DATABASE_PATH, null);
			try {
				database.execSQL(SQL_MESSAGES);
			} catch (Exception e) {
				Log.e("PrivateMessageDatabase", e.getMessage());
			}
		} else {
			database = SQLiteDatabase.openDatabase(
					PathDefine.MESSAGE_DATABASE_PATH, null,
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
					null, "_id desc", "0,500");
		}
		return c;
	}

	public Cursor queryLastMessageId() {
		Cursor c = null;
		if (database != null) {
			c = database.query(TABLE_MESSAGES, new String[] { "id" }, null,
					null, null, null, "_id desc", "0,1");
		}
		return c;
	}

	public void insertMessage(ContentValues cv) {
		if (database != null) {
			database.insert(TABLE_MESSAGES, null, cv);
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

}
