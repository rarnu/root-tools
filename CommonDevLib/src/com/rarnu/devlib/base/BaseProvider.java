package com.rarnu.devlib.base;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;

public abstract class BaseProvider extends ContentProvider {

    public abstract String getUriName();

    public abstract BaseDatabase createDatabase(Context context);

    public abstract Cursor doQuery(BaseDatabase database, int actionId, Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder);

    public abstract int doUpdate(BaseDatabase database, int actionId, Uri uri, ContentValues values, String selection, String[] selectionArgs);

    public abstract Uri doInsert(BaseDatabase database, int actionId, Uri uri, ContentValues values);

    public abstract int doDelete(BaseDatabase database, int actionId, Uri uri, String selection, String[] selectionArgs);

    public BaseDatabase database = null;
    public static Uri CONTENT_URI = null;

    @Override
    public int delete(Uri uri, String selection, String[] selectionArgs) {
        int actionId = extractActionId(uri);
        int ret = -1;
        if (database != null) {
            ret = doDelete(database, actionId, uri, selection, selectionArgs);
        }
        return ret;
    }

    public abstract String getType(Uri uri);

    @Override
    public Uri insert(Uri uri, ContentValues values) {
        int actionId = extractActionId(uri);
        Uri u = null;
        if (database != null) {
            u = doInsert(database, actionId, uri, values);
        }
        return u;
    }

    @Override
    public boolean onCreate() {
        CONTENT_URI = Uri.parse("content://" + getUriName());
        if (database == null) {
            try {
                database = createDatabase(getContext());
            } catch (Exception e) {
            }
        }
        return (database != null);
    }

    @Override
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {

        int actionId = extractActionId(uri);

        Cursor c = null;
        if (database != null) {
            c = doQuery(database, actionId, uri, projection, selection, selectionArgs, sortOrder);
        }

        return c;
    }

    @Override
    public int update(Uri uri, ContentValues values, String selection, String[] selectionArgs) {
        int actionId = extractActionId(uri);
        int ret = -1;
        if (database != null) {
            ret = doUpdate(database, actionId, uri, values, selection, selectionArgs);
        }
        return ret;
    }

    private int extractActionId(Uri uri) {
        int actionId = -1;
        try {
            actionId = (int) ContentUris.parseId(uri);
        } catch (Exception e) {

        }
        return actionId;
    }

}
