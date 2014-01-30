package com.rarnu.tools.root.database;

import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.util.Log;
import com.rarnu.devlib.base.BaseDatabase;
import com.rarnu.devlib.base.BaseProvider;

public class PasswordProvider extends BaseProvider {

    public static final String URI_NAME = "com.rarnu.tools.root.password";
    public static final int ACTION_QUERY_SECPWD = 10;
    public static final int ACTION_QUERY_PASSWORD_LIST = 11;
    public static final int ACTION_UPDATE_SECPWD = 20;
    public static final int ACTION_UPDATE_PASSWORD = 21;
    public static final int ACTION_INSERT_PASSWORD = 30;
    public static final int ACTION_DELETE_PASSWORD = 40;

    @Override
    public String getUriName() {
        return URI_NAME;
    }

    @Override
    public BaseDatabase createDatabase(Context context) {
        PasswordDatabase database = null;
        try {
            database = new PasswordDatabase(context);
        } catch (Exception e) {
        }
        Log.e("createDatabase", "database is " + (database == null ? "null" : "not null"));
        return database;
    }

    @Override
    public Cursor doQuery(BaseDatabase database, int actionId, Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        Cursor c = null;
        switch (actionId) {
            case ACTION_QUERY_SECPWD:
                c = database.query(PasswordDatabase.TABLE_SEC, null, selection, selectionArgs, null, null, null);
                break;
            case ACTION_QUERY_PASSWORD_LIST:
                c = database.query(PasswordDatabase.TABLE_PWD, null, null, null, null, null, null);
                break;
        }
        return c;
    }

    @Override
    public int doUpdate(BaseDatabase database, int actionId, Uri uri, ContentValues values, String selection, String[] selectionArgs) {
        int ret = -1;
        switch (actionId) {
            case ACTION_UPDATE_SECPWD:
                ret = database.update(PasswordDatabase.TABLE_SEC, values, null, null);
                break;
            case ACTION_UPDATE_PASSWORD:
                ret = database.update(PasswordDatabase.TABLE_PWD, values, selection, selectionArgs);
                break;
        }
        return ret;
    }

    @Override
    public Uri doInsert(BaseDatabase database, int actionId, Uri uri, ContentValues values) {
        Uri ret = null;
        switch (actionId) {
            case ACTION_INSERT_PASSWORD:
                long insertRet = database.insert(PasswordDatabase.TABLE_PWD, values);
                if (insertRet != -1) {
                    ret = ContentUris.withAppendedId(uri, insertRet);
                }
                break;
        }
        return ret;
    }

    @Override
    public int doDelete(BaseDatabase database, int actionId, Uri uri, String selection, String[] selectionArgs) {
        int ret = -1;
        switch (actionId) {
            case ACTION_DELETE_PASSWORD:
                ret = database.delete(PasswordDatabase.TABLE_PWD, selection, selectionArgs);
                break;
        }
        return ret;
    }

    @Override
    public String getType(Uri uri) {
        return null;
    }

}
