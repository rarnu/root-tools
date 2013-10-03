package com.yugioh.android.database;

import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.net.Uri;
import com.yugioh.android.R;
import com.yugioh.android.define.PathDefine;

import java.io.File;

public class YugiohDatabase {

    private SQLiteDatabase database;

    public YugiohDatabase(Context context) throws Exception {
        // this.context = context;
        String dbName = PathDefine.DATABASE_PATH;
        File fDb = new File(dbName);
        if (!fDb.exists()) {
            throw new Exception(context.getResources().getString(R.string.error_no_database));
        }
        database = SQLiteDatabase.openDatabase(dbName, null, SQLiteDatabase.OPEN_READONLY);

    }

    public static boolean isDatabaseFileExists() {
        return new File(PathDefine.DATABASE_PATH).exists();
    }

    public Cursor doQuery(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        int actionId = -99;
        try {
            actionId = (int) ContentUris.parseId(uri);
        } catch (Exception e) {

        }
        Cursor c = null;
        if (database != null) {
            switch (actionId) {
                case YugiohProvider.ACTIONID_CARDCOUNT:
                    c = database.rawQuery("select CardID from YGODATA order by CardID desc limit 0,1", null);
                    break;
                case YugiohProvider.ACTIONID_EFFECTLIST:
                    c = database.rawQuery("select * from YGOEFFECT", null);
                    break;
                case YugiohProvider.ACTIONID_TOP100:
                    c = database.rawQuery("select _id, SCCardName, SCCardType from YGODATA order by _id desc limit 0,100 ", null);
                    break;
                case YugiohProvider.ACTIONID_SEARCH:
                    c = database.query("YGODATA", projection, selection, selectionArgs, null, null, sortOrder);
                    break;
                default:
                    if (actionId >= 0) {
                        c = database.rawQuery("select * from YGODATA where CardID=?", new String[]{String.valueOf(actionId)});
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
