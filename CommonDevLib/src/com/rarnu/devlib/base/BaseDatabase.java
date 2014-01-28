package com.rarnu.devlib.base;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.CancellationSignal;

import java.io.File;
import java.util.List;

public abstract class BaseDatabase {

    private SQLiteDatabase database;

    public abstract String getDatabasePath();

    public abstract List<String> getListSqlCreateTables();

    public BaseDatabase(Context context) throws Exception {
        String dbName = getDatabasePath();
        File fDb = new File(dbName);
        if (!fDb.exists()) {
            database = SQLiteDatabase.openOrCreateDatabase(dbName, null);
            List<String> listSql = getListSqlCreateTables();
            if (listSql != null && listSql.size() != 0) {
                for (String sql : listSql) {
                    try {
                        database.execSQL(sql);
                    } catch (Exception e) {

                    }
                }
            }
            database.close();
            database = null;
        }
        database = SQLiteDatabase.openDatabase(dbName, null, SQLiteDatabase.OPEN_READWRITE);

    }

    public Cursor query(String table, String[] columns, String selection, String[] selectionArgs, String groupBy, String having, String orderBy) {
        Cursor c = null;
        if (database != null) {
            c = database.query(table, columns, selection, selectionArgs, groupBy, having, orderBy);
        }
        return c;
    }

    public Cursor query(String table, String[] columns, String selection, String[] selectionArgs, String groupBy, String having, String orderBy, String limit) {
        Cursor c = null;
        if (database != null) {
            c = database.query(false, table, columns, selection, selectionArgs, groupBy, having, orderBy, limit);
        }
        return c;
    }

    public Cursor query(boolean distinct, String table, String[] columns, String selection, String[] selectionArgs, String groupBy, String having, String orderBy, String limit) {
        Cursor c = null;
        if (database != null) {
            c = database.query(distinct, table, columns, selection, selectionArgs, groupBy, having, orderBy, limit);
        }
        return c;
    }

    public Cursor query(boolean distinct, String table, String[] columns, String selection, String[] selectionArgs, String groupBy, String having, String orderBy, String limit, CancellationSignal cancellationSignal) {
        Cursor c = null;
        if (database != null) {
            c = database.query(distinct, table, columns, selection, selectionArgs, groupBy, having, orderBy, limit, cancellationSignal);
        }
        return c;
    }

    public Cursor rawQuery(String sql) {
        Cursor c = null;
        if (database != null) {
            c = database.rawQuery(sql, null);
        }
        return c;
    }

    public Cursor rawQuery(String sql, String[] args) {
        Cursor c = null;
        if (database != null) {
            c = database.rawQuery(sql, args);
        }
        return c;
    }

    public long insert(String table, ContentValues values) {
        long ret = -1;
        if (database != null) {
            try {
                ret = database.insert(table, null, values);
            } catch (Exception e) {

            }
        }

        return ret;
    }

    public int update(String table, ContentValues values, String whereClause, String[] whereArgs) {
        int ret = -1;
        if (database != null) {
            try {
                ret = database.update(table, values, whereClause, whereArgs);
            } catch (Exception e) {

            }
        }
        return ret;
    }

    public int delete(String table, String whereClause, String[] whereArgs) {
        int ret = -1;
        if (database != null) {
            try {
                ret = database.delete(table, whereClause, whereArgs);
            } catch (Exception e) {

            }
        }
        return ret;
    }

    public void execSQL(String sql) {
        if (database != null) {
            try {
                database.execSQL(sql);
            } catch (Exception e) {

            }
        }
    }

    public void close() {
        if (database != null) {
            database.close();
        }
    }

}
