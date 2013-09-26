package com.rarnu.utils;

import android.content.Context;

public class ResourceUtils {

    private static Context mContext;

    public static void init(Context context) {
        mContext = context;
    }

    public static String getString(int res) {
        return mContext.getString(res);
    }

    public static String getString(int res, Object... params) {
        return mContext.getString(res, params);
    }

    public static String[] getStringArray(int res) {
        return mContext.getResources().getStringArray(res);
    }
}
