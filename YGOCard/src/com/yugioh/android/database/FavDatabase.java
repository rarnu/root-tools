package com.yugioh.android.database;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import com.yugioh.android.define.PathDefine;

import java.io.File;

public class FavDatabase {

    private static final String CREATE_TABLE_FAV = "create table fav(cardId int primary key)";
    private static final String TABLE_FAV = "fav";
    private SQLiteDatabase database;

    public FavDatabase(Context context) throws Exception {
        // this.context = context;
        String dbName = PathDefine.FAV_DATABASE_NAME;
        File fDb = new File(dbName);
        if (!fDb.exists()) {
            database = SQLiteDatabase.openOrCreateDatabase(fDb, null);
            database.execSQL(CREATE_TABLE_FAV);
        } else {
            database = SQLiteDatabase.openDatabase(dbName, null, SQLiteDatabase.OPEN_READWRITE);
        }
    }

    public void addFav(int id) {
        if (database != null) {
            ContentValues cv = new ContentValues();
            cv.put("cardId", id);
            try {
                database.insert(TABLE_FAV, null, cv);
            } catch (Exception e) {

            }
        }
    }

    public void removeFav(int id) {
        if (database != null) {
            try {
                database.delete(TABLE_FAV, String.format("cardId=%d", id), null);
            } catch (Exception e) {

            }
        }
    }

    public Cursor queryFav(int id) {
        Cursor c = null;
        if (database != null) {
            c = database.query(TABLE_FAV, null, String.format("cardId=%d", id), null, null, null, null);
        }
        return c;
    }

    public Cursor queryAllFav() {
        Cursor c = null;
        if (database != null) {
            c = database.query(TABLE_FAV, null, null, null, null, null, null);
        }
        return c;
    }
}
