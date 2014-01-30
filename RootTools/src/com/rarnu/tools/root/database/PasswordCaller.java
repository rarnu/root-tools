package com.rarnu.tools.root.database;

import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import com.rarnu.tools.root.common.PasswordItem;

import java.util.ArrayList;
import java.util.List;

public class PasswordCaller {

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
                int u = context.getContentResolver().update(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_UPDATE_SECPWD), cv, null, null);
                if (u != -1) {
                    ret = true;
                }
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
                while (!c.isAfterLast()) {
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

    public static boolean addPassword(Context context, String name, String account, String password, String memo) {
        boolean ret = false;
        if (context != null) {
            ContentValues cv = new ContentValues();
            cv.put("c_name", name);
            cv.put("c_account", account);
            cv.put("c_password", password);
            cv.put("c_memo", memo);
            Uri u = context.getContentResolver().insert(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_INSERT_PASSWORD), cv);
            if (u != null) {
                ret = true;
            }
        }
        return ret;
    }

    public static boolean updatePassword(Context context, int id, String name, String account, String password, String memo) {
        boolean ret = false;
        if (context != null) {
            ContentValues cv = new ContentValues();
            cv.put("c_name", name);
            cv.put("c_account", account);
            cv.put("c_password", password);
            cv.put("c_memo", memo);
            int u = context.getContentResolver().update(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_UPDATE_PASSWORD), cv, "c_id=?", new String[]{String.valueOf(id)});
            if (u != -1) {
                ret = true;
            }
        }
        return ret;
    }

    public static boolean deletePassword(Context context, int id) {
        boolean ret = false;
        if (context != null) {
            int d = context.getContentResolver().delete(ContentUris.withAppendedId(PasswordProvider.CONTENT_URI, PasswordProvider.ACTION_DELETE_PASSWORD), "c_id=?", new String[]{String.valueOf(id)});
            if (d != -1) {
                ret = true;
            }
        }
        return ret;
    }

}
