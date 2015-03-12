package com.rarnu.utils;

import android.content.ContentValues;
import android.database.Cursor;
import java.lang.reflect.Field;
import java.util.List;

public class CursorUtils {

    public static ContentValues buildContentValues(Object obj) {
        ContentValues cv = new ContentValues();
        try {
            Field[] fs = obj.getClass().getFields();
            String typeStr = "";
            for (Field f : fs) {
                typeStr = f.getType().getSimpleName();
                if (typeStr.equals("String")) {
                    cv.put(f.getName(), (String) f.get(obj));
                } else if (typeStr.equals("int")) {
                    cv.put(f.getName(), f.getInt(obj));
                } else if (typeStr.equals("double")) {
                    cv.put(f.getName(), f.getDouble(obj));
                } else if (typeStr.equals("boolean")) {
                    cv.put(f.getName(), f.getBoolean(obj));
                } else if (typeStr.equals("float")) {
                    cv.put(f.getName(), f.getFloat(obj));
                } else if (typeStr.equals("long")) {
                    cv.put(f.getName(), f.getLong(obj));
                } else if (typeStr.equals("byte")) {
                    cv.put(f.getName(), f.getByte(obj));
                } else if (typeStr.equals("short")) {
                    cv.put(f.getName(), f.getShort(obj));
                } else if (typeStr.equals("char")) {
                    cv.put(f.getName(), String.valueOf(f.getChar(obj)));
                }
            }
        } catch (Exception e) {

        }
        return cv;
    }

    public static void fillCursorToObject(Cursor c, Object obj) {
        try {
            Field[] fs = obj.getClass().getFields();
            String typeStr = "";
            for (Field f : fs) {
                typeStr = f.getType().getSimpleName();
                if (typeStr.equals("String")) {
                    f.set(obj, c.getString(c.getColumnIndex(f.getName())));
                } else if (typeStr.equals("int")) {
                    f.setInt(obj, c.getInt(c.getColumnIndex(f.getName())));
                } else if (typeStr.equals("double")) {
                    f.setDouble(obj, c.getDouble(c.getColumnIndex(f.getName())));
                } else if (typeStr.equals("boolean")) {
                    f.setBoolean(obj, c.getInt(c.getColumnIndex(f.getName())) != 0);
                } else if (typeStr.equals("float")) {
                    f.setFloat(obj, c.getFloat(c.getColumnIndex(f.getName())));
                } else if (typeStr.equals("long")) {
                    f.setLong(obj, c.getLong(c.getColumnIndex(f.getName())));
                } else if (typeStr.equals("byte")) {
                    f.setByte(obj, Byte.valueOf(c.getString(c.getColumnIndex(f.getName()))));
                } else if (typeStr.equals("short")) {
                    f.setShort(obj, c.getShort(c.getColumnIndex(f.getName())));
                } else if (typeStr.equals("char")) {
                    f.setChar(obj, c.getString(c.getColumnIndex(f.getName())).charAt(0));
                }
            }
        } catch (Exception e) {

        }
    }

    public static String buildSplittedString(List<?> list, String splitter) {
        String result = "";
        for (Object o : list) {
            result += String.format("%s,", o.toString());
        }
        if (result.length() > 0) {
            result = result.substring(0, result.length() - 1);
        }
        return result;
    }

}
