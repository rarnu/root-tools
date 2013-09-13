package com.rarnu.adcenter.database;

import java.io.File;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;

public class AdDatabase {

	private SQLiteDatabase database = null;
	private String databaseFileName = "";

	private static final String CREATE_TABLE_AD = "create table ad(id int primary key, quest_answered int not null)";
	private static final String CREATE_TABLE_USER = "create table user(id int primary key, name text not null, account text not null, cash int not null)";
	private static final String TABLE_AD = "ad";
	private static final String TABLE_USER = "user";

	public AdDatabase(Context context) throws Exception {
		databaseFileName = "/data/data/" + context.getPackageName()
				+ "/databases/";
		if (!new File(databaseFileName).exists()) {
			new File(databaseFileName).mkdirs();
		}
		databaseFileName += "ad.db";
		if (new File(databaseFileName).exists()) {
			database = SQLiteDatabase.openDatabase(databaseFileName, null,
					SQLiteDatabase.OPEN_READWRITE);
		} else {
			database = SQLiteDatabase.openOrCreateDatabase(databaseFileName,
					null);
			database.execSQL(CREATE_TABLE_AD);
			database.execSQL(CREATE_TABLE_USER);
		}
		if (database == null) {
			throw new Exception("open database failed.");
		}
	}

	public Cursor queryAdQuested(String selection, String[] args) {
		Cursor c = null;
		if (database != null) {
			c = database.query(TABLE_AD, new String[] { "quest_answered" },
					selection, args, null, null, null);
		}
		return c;
	}

	public void setAdQuested(ContentValues cv, String selection, String[] args) {
		if (database != null) {
			database.update(TABLE_AD, cv, selection, args);
		}
	}

	public void saveAd(ContentValues cv) {
		if (database != null) {
			try {
				if (!adExists(cv.getAsInteger("id"))) {
					database.insert(TABLE_AD, null, cv);
				}
			} catch (Exception e) {

			}
		}
	}

	private boolean adExists(int id) {
		boolean ret = false;
		if (database != null) {
			Cursor c = database.query(TABLE_AD, new String[] { "id" }, "id=?",
					new String[] { String.valueOf(id) }, null, null, null);
			if (c != null) {
				c.moveToFirst();
				while (!c.isAfterLast()) {
					ret = true;
					break;
				}
				c.close();
			}
		}
		return ret;
	}

	public void login(ContentValues cv) {
		if (database != null) {
			try {
				database.insert(TABLE_USER, null, cv);
			} catch (Exception e) {

			}
		}
	}

	public void logout(String selection, String[] args) {
		if (database != null) {
			try {
				database.delete(TABLE_USER, selection, args);
			} catch (Exception e) {

			}
		}
	}

	public Cursor queryUser(String selection, String[] args) {
		Cursor c = null;
		if (database != null) {
			c = database.query(TABLE_USER, null, selection, args, null, null,
					null);
		}
		return c;
	}
	
	public void updateCash(ContentValues cv, String selection, String[] args) {
		if (database != null) {
			database.update(TABLE_USER, cv, selection, args);
		}
	}
}
