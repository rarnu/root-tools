package com.yugioh.android.database;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Handler;
import android.os.Message;
import com.rarnu.utils.FileUtils;
import com.yugioh.android.define.PathDefine;

import java.io.File;

public class FavDatabase {

    private Context context;

    private static final String TABLE_FAV = "fav";
    private SQLiteDatabase database;
    private Handler hCopy = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == FileUtils.WHAT_COPY_FINISH) {
                database = SQLiteDatabase.openDatabase(PathDefine.FAV_DATABASE_NAME, null, SQLiteDatabase.OPEN_READWRITE);
            }
            super.handleMessage(msg);
        }
    };

    public FavDatabase(Context context) throws Exception {
        this.context = context;
        File fDb = new File(PathDefine.FAV_DATABASE_NAME);
        if (!fDb.exists()) {
            asyncCopy(context);
        } else {
            database = SQLiteDatabase.openDatabase(PathDefine.FAV_DATABASE_NAME, null, SQLiteDatabase.OPEN_READWRITE);
        }
    }

    private void asyncCopy(Context context) {
        FileUtils.copyAssetFile(context, "fav.db", PathDefine.ROOT_PATH, hCopy);
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
