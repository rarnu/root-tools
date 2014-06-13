package com.yugioh.android.database;

import android.content.ContentUris;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.net.Uri;
import android.os.Handler;
import android.os.Message;
import com.rarnu.utils.FileUtils;
import com.yugioh.android.common.Actions;
import com.yugioh.android.define.PathDefine;

import java.io.File;

public class YugiohDatabase {

    private Context context;
    private SQLiteDatabase database;

    private Handler hCopy = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == FileUtils.WHAT_COPY_FINISH) {
                database = SQLiteDatabase.openDatabase(PathDefine.DATABASE_PATH, null, SQLiteDatabase.OPEN_READONLY);
                context.sendBroadcast(new Intent(Actions.ACTION_EXTRACT_DATABASE_COMPLETE));
            }
            super.handleMessage(msg);
        }
    };

    public YugiohDatabase(Context context) throws Exception {
        this.context = context;
        File fDb = new File(PathDefine.DATABASE_PATH);
        if (!fDb.exists()) {
            asyncCopy(context);
        } else {
            database = SQLiteDatabase.openDatabase(PathDefine.DATABASE_PATH, null, SQLiteDatabase.OPEN_READONLY);
        }
    }

    public static boolean isDatabaseFileExists() {
        return new File(PathDefine.DATABASE_PATH).exists();
    }

    private void asyncCopy(Context context) {
        context.sendBroadcast(new Intent(Actions.ACTION_EXTRACT_DATABASE));
        FileUtils.copyAssetFile(context, "yugioh.db", PathDefine.ROOT_PATH, hCopy);
    }

    public Cursor doQuery(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        int actionId = -99;
        try {
            actionId = (int) ContentUris.parseId(uri);
        } catch (Exception e) {

        }
        Cursor c = null;
        if (database != null && database.isOpen()) {
            switch (actionId) {
                case YugiohProvider.ACTIONID_CARDCOUNT:
                    c = database.rawQuery("select _id from ygodata order by _id desc limit 0,1", null);
                    break;
                case YugiohProvider.ACTIONID_TOP100:
                    c = database.rawQuery("select _id, name, sCardType from ygodata order by _id desc limit 0,100 ", null);
                    break;
                case YugiohProvider.ACTIONID_SEARCH:
                    c = database.query("ygodata", projection, selection, selectionArgs, null, null, sortOrder);
                    break;
                default:
                    if (actionId >= 0) {
                        c = database.rawQuery("select * from ygodata where _id=?", new String[]{String.valueOf(actionId)});
                    }
            }
        }
        return c;
    }

    public void close() {
        if (database != null) {
            database.close();
        }
    }

    public Cursor doGetVersion() {
        Cursor c = null;
        if (database != null) {
            try {
                c = database.rawQuery("select * from version", null);
            } catch (Exception e) {

            }
        }
        return c;
    }

}
