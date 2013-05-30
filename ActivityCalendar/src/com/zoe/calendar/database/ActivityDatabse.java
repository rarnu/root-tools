package com.zoe.calendar.database;

import java.io.File;

import android.content.ContentValues;
import android.content.Context;
import android.content.ContextWrapper;
import android.content.Intent;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Handler;
import android.os.Message;

import com.rarnu.utils.FileUtils;
import com.zoe.calendar.common.Actions;

public class ActivityDatabse extends ContextWrapper {

	private String DB_FILE_PATH = "";
	private String DB_FILE_NAME = "";
	SQLiteDatabase db;
	public static final String ACTIVITY_TABLE = "activity";
	public static final String MOTION_TABLE = "motion";

	public ActivityDatabse(Context context) {
		super(context);
		DB_FILE_PATH = "/data/data/" + getPackageName() + "/databases/";
		DB_FILE_NAME = DB_FILE_PATH + "activity.db";
		if (!new File(DB_FILE_PATH).exists()) {
			copyDatabaseT();
		} else {
			loadDatabase();
		}
	}

	private void loadDatabase() {
		try {
			db = SQLiteDatabase.openDatabase(DB_FILE_NAME, null,
					SQLiteDatabase.OPEN_READWRITE);
		} catch (Exception e) {
			db = null;
		}

		sendBroadcast(new Intent(Actions.ACTION_LOAD_DATABASE_FINISH));
	}

	public void releaseDatabase() {
		if (db != null) {
			db.close();
		}
	}

	private void copyDatabaseT() {
		final Handler hOpenDB = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					loadDatabase();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				File fDatabaseDir = new File(DB_FILE_PATH);
				if (!fDatabaseDir.exists()) {
					fDatabaseDir.mkdirs();
				}
				FileUtils.copyAssetFile(ActivityDatabse.this, "activity.db",
						DB_FILE_PATH, null);
				hOpenDB.sendEmptyMessage(1);
			}
		}).start();
	}

	public Cursor query(String table, String selection, String[] selectionArgs) {

		if (db != null) {
			if (table.equals(MOTION_TABLE)) {
				return db.query(table, null, selection, selectionArgs, null,
						null, "_id desc", "0, 10");
			} else {
				return db.query(true, table, null, selection, selectionArgs,
						null, null, null, null);
			}
		} else {
			return null;
		}
	}

	public int updateStatus(ContentValues cv, String[] whereArgs) {
		if (db != null) {
			return db.update(ACTIVITY_TABLE, cv, "_id=?", whereArgs);
		} else {
			return 0;
		}
	}

	public int updateMotion(ContentValues cv, String where, String[] whereArgs) {
		if (db != null) {
			return db.update(MOTION_TABLE, cv, where, whereArgs);
		} else {
			return 0;
		}
	}

	public long insert(String table, ContentValues cv) {
		if (db != null) {
			if (table.equals(MOTION_TABLE)) {
				cv.put("_id", generateId(MOTION_TABLE));
				return db.insert(table, null, cv);
			} else {
				return db.insert(table, null, cv);
			}
		} else {
			return 0L;
		}
	}

	private int generateId(String table) {
		if (db != null) {
			Cursor c = db.query(table, new String[] { "_id" }, null, null,
					null, null, "_id desc", "0,1");
			int id = 0;
			if (c == null) {
				id = 1;
			} else {
				c.moveToFirst();
				while (!c.isAfterLast()) {
					id = c.getInt(0);
					break;
				}
				id++;
				c.close();
			}
			return id;
		} else {
			return 0;
		}
	}
}
