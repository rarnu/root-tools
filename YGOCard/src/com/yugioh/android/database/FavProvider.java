package com.yugioh.android.database;

import android.content.ContentProvider;
import android.content.ContentUris;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

public class FavProvider extends ContentProvider {

    public static final Uri CONTENT_URI = Uri.parse("content://com.yugioh.fav");
    public static final int ACTION_QUERY = 1;
    public static final int ACTION_QUERY_ALL = 2;
    FavDatabase database = null;

    @Override
    public int delete(Uri uri, String selection, String[] selectionArgs) {
        if (database != null) {
            database.removeFav((int) ContentUris.parseId(uri));
        }
        return 0;
    }

    @Override
    public String getType(Uri uri) {
        return null;
    }

    @Override
    public Uri insert(Uri uri, ContentValues values) {
        if (database != null) {
            database.addFav((int) ContentUris.parseId(uri));
        }
        return null;
    }

    @Override
    public boolean onCreate() {
        if (database == null) {
            try {
                database = new FavDatabase(getContext());
            } catch (Exception e) {

            }
        }
        return (database != null);
    }

    @Override
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        int actionId = -99;
        try {
            actionId = (int) ContentUris.parseId(uri);
        } catch (Exception e) {

        }
        Cursor c = null;
        if (database != null) {
            switch (actionId) {
                case ACTION_QUERY:
                    c = database.queryFav(Integer.parseInt(selection));
                    break;
                case ACTION_QUERY_ALL:
                    c = database.queryAllFav();
                    break;
            }
        }
        return c;
    }

    @Override
    public int update(Uri uri, ContentValues values, String selection, String[] selectionArgs) {
        return 0;
    }
}
