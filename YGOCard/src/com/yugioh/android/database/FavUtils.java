package com.yugioh.android.database;

import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;

public class FavUtils {

    public static void addFav(Context context, int id) {
        context.getContentResolver().insert(ContentUris.withAppendedId(FavProvider.CONTENT_URI, id), null);
    }

    public static void removeFav(Context context, int id) {
        context.getContentResolver().delete(ContentUris.withAppendedId(FavProvider.CONTENT_URI, id), null, null);
    }

    public static boolean queryFav(Context context, int id) {
        boolean ret = false;
        Cursor c = context.getContentResolver().query(ContentUris.withAppendedId(FavProvider.CONTENT_URI, FavProvider.ACTION_QUERY), null, String.valueOf(id), null, null);
        if (c != null) {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                ret = true;
                c.moveToNext();
            }
            c.close();
        }
        return ret;
    }

    public static Cursor queryAllFav(Context context) {
        Cursor c = context.getContentResolver().query(ContentUris.withAppendedId(FavProvider.CONTENT_URI, FavProvider.ACTION_QUERY_ALL), null, null, null, null);
        return c;
    }

}
