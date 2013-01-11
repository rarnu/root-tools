package com.rarnu.vim.emotion.database;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;

public class EmotionDatabase {

	private static final String CREATE_TABLE_EMOTION = "create table emotion(id int primary key, year int, month int, day int, emotion text, comment text)";
	private static final String INSERT_EMOTION = "insert into emotion (id, year, month, day, emotion, comment) values (%d, %d, %d, %d, '%s', '%s')";
	
	private SQLiteDatabase db = null;

	public EmotionDatabase(Context context) {
		String dbfn = "/data/data/" + context.getPackageName() + "/data.db";
		File fDb = new File(dbfn);
		if (!fDb.exists()) {
			db = SQLiteDatabase.openOrCreateDatabase(fDb, null);
			db.execSQL(CREATE_TABLE_EMOTION);
		} else {
			db = SQLiteDatabase.openOrCreateDatabase(fDb, null);
		}
	}

	public void close() {
		db.close();
	}

	public void insertEmotion(long stamp, String emotion, String comment) {
		int id = generateId("emotion");
		Calendar c = Calendar.getInstance();
		c.setTimeInMillis(stamp);
		int year = c.get(Calendar.YEAR);
		int month = c.get(Calendar.MONTH);
		int day = c.get(Calendar.DAY_OF_MONTH);
		String sql = String.format(INSERT_EMOTION, id, year, month, day, emotion, comment);
		try {
			db.execSQL(sql);
		} catch (Exception e) {

		}
	}

	public EmotionInfo queryEmotion(int id) {
		EmotionInfo info = new EmotionInfo();

		Cursor c = db.query("emotion", null, "id=?",
				new String[] { String.valueOf(id) }, "id", null, "id desc");
		if (c != null) {
			c.moveToFirst();
			while (!c.isAfterLast()) {
				info.day = c.getInt(0);
				info.year = c.getInt(1);
				info.month = c.getInt(2);
				info.day = c.getInt(3);
				info.emotion = c.getString(4);
				info.comment = c.getString(5);
				break;
			}
			c.close();
		}

		return info;
	}

	public List<EmotionInfo> queryHistory() {
		List<EmotionInfo> list = new ArrayList<EmotionInfo>();
		Cursor c = db.query("emotion", null, null, null, "id", null, "id desc");
		if (c != null) {
			c.moveToFirst();
			while (!c.isAfterLast()) {
				EmotionInfo info = new EmotionInfo();
				info.day = c.getInt(0);
				info.year = c.getInt(1);
				info.month = c.getInt(2);
				info.day = c.getInt(3);
				info.emotion = c.getString(4);
				info.comment = c.getString(5);
				list.add(info);
				c.moveToNext();
			}
			c.close();
		}

		return list;
	}

	private int generateId(String table) {
		int ret = 0;
		Cursor c = db.query(table, new String[] { "id" }, null, null, null,
				null, "id desc", "0,1");
		if (c != null) {
			c.moveToFirst();
			while (!c.isAfterLast()) {
				ret = c.getInt(c.getColumnIndex("id")) + 1;
				break;
			}
			c.close();
		}
		return ret;
	}

}
