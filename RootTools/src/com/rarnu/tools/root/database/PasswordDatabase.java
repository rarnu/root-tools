package com.rarnu.tools.root.database;

import android.content.Context;
import com.rarnu.devlib.base.BaseDatabase;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.FileUtils;

import java.util.ArrayList;
import java.util.List;

public class PasswordDatabase extends BaseDatabase {

    public static final String TABLE_SEC = "sec";
    public static final String TABLE_PWD = "pwd";
    private static String SEC_DB = "sec.db";

    public PasswordDatabase(Context context) throws Exception {
        super(context);
    }

    @Override
    public String getDatabasePath(Context context) {
        return DirHelper.PASSWORD_DIR;
    }

    @Override
    public String getDatabaseFileName(Context context) {
        return SEC_DB;
    }

    @Override
    public List<String> getListSqlCreateTables() {
        List<String> list = new ArrayList<String>();
        list.add("create table sec(c_password int not null)");
        list.add("create table pwd(c_id integer primary key autoincrement, c_name text not null, c_account text, c_password text, c_memo text)");
        list.add("insert into sec(c_password) values ('0000')");
        return list;
    }

    @Override
    public boolean isCopyDatabase() {
        return true;
    }

    @Override
    public void copyDatabaseFile(Context context, String path, String name) {
        FileUtils.copyAssetFile(context, name, path, null);
    }
}
