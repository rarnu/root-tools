package com.rarnu.tools.root.database;

import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import com.rarnu.tools.root.common.PasswordItem;

import java.util.ArrayList;
import java.util.List;

public class PasswordCaller {

    public static void createOrOpenDatabase(Context context) {
        if (context != null) {
            context.getContentResolver().query(PasswordProvider.CONTENT_URI, null, null, null, null);
        }
    }

    public static void destroyDatabase(Context context) {
        if (context != null) {
            context.getContentResolver().query(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_DESTROY), null, null, null, null);
        }
    }

    public static boolean isInitSecPassword(Context context) {
        boolean ret = false;
        if (context != null) {
            Cursor c = context.getContentResolver().query(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_QUERY_SECPWD), null, "c_password=?", new String[]{"0000"}, null);
            if (c != null) {
                c.moveToFirst();
                while (!c.isAfterLast()) {
                    ret = true;
                    c.moveToNext();
                }
                c.close();
            }
        }
        return ret;
    }

    public static boolean isPasswordRight(Context context, String pwd) {
        boolean ret = false;
        if (context != null) {
            Cursor c = context.getContentResolver().query(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_QUERY_SECPWD), null, "c_password=?", new String[]{pwd}, null);
            if (c != null) {
                c.moveToFirst();
                while (!c.isAfterLast()) {
                    ret = true;
                    c.moveToNext();
                }
                c.close();
            }
        }
        return ret;
    }

    public static boolean changeSecPassword(Context context, String oldPwd, String newPwd) {
        boolean ret = false;
        if (context != null) {
            Cursor c = context.getContentResolver().query(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_QUERY_SECPWD), null, "c_password=?", new String[]{oldPwd}, null);
            boolean rightOld = false;
            if (c != null) {
                c.moveToFirst();
                while (!c.isAfterLast()) {
                    rightOld = true;
                    c.moveToNext();
                }
                c.close();
            }
            if (rightOld) {
                ContentValues cv = new ContentValues();
                cv.put("c_password", newPwd);
                context.getContentResolver().update(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_UPDATE_SECPWD), cv, null, null);
                ret = true;
            }
        }

        return ret;
    }

    public static List<PasswordItem> getPasswordList(Context context) {
        List<PasswordItem> list = null;
        if (context != null) {
            Cursor c = context.getContentResolver().query(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_QUERY_PASSWORD_LIST), null, null, null, null);
            if (c != null) {
                list = new ArrayList<PasswordItem>();
                c.moveToFirst();
                if (!c.isAfterLast()) {
                    PasswordItem item = new PasswordItem();
                    item.id = c.getInt(c.getColumnIndex("c_id"));
                    item.name = c.getString(c.getColumnIndex("c_name"));
                    item.account = c.getString(c.getColumnIndex("c_account"));
                    item.password = c.getString(c.getColumnIndex("c_password"));
                    item.memo = c.getString(c.getColumnIndex("c_memo"));
                    list.add(item);
                    c.moveToNext();
                }
                c.close();
            }
        }
        return list;
    }

}
