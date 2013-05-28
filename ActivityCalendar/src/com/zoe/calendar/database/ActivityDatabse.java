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
	private static final String ACTIVITY_TABLE = "activity";

	public ActivityDatabse(Context context) {
		super(context);
		DB_FILE_PATH = "/data/data/" + getPackageName() + "/database/";
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

	public Cursor query(String selection, String[] selectionArgs) {
		if (db != null) {
			return db.query(true, ACTIVITY_TABLE, null, selection,
					selectionArgs, null, null, null, null);
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
	
	public long insert(ContentValues cv) {
		return db.insert(ACTIVITY_TABLE, null, cv);
	}
}
