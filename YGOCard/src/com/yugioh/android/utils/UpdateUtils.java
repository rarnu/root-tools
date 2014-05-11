package com.yugioh.android.utils;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import com.rarnu.utils.FileUtils;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.database.YugiohUtils;
import com.yugioh.android.define.PathDefine;

public class UpdateUtils {

    public static void checkUpdateT(final Context context, final Handler hHint) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                int dbVer = YugiohUtils.getDatabaseVersion(context);
                int lastCardId = YugiohUtils.getLastCardId(context);
                UpdateInfo ui = YGOAPI.findUpdate(context, dbVer, lastCardId);
                Message msg = new Message();
                msg.what = 1;
                msg.obj = ui;
                hHint.sendMessage(msg);
            }
        }).start();
    }

    public static void updateLocalDatabase(final Context context, final Handler hProgress) {
        new Thread(new Runnable() {
            @Override
            public void run() {

                int dbVer = YugiohUtils.getDatabaseVersion(context);
                if (dbVer < 100) {
                    if (hProgress != null) {
                        hProgress.sendEmptyMessage(0);
                    }
                    YugiohUtils.closeDatabase(context);
                    FileUtils.deleteFile(PathDefine.DATABASE_PATH);
                    FileUtils.copyAssetFile(context, "yugioh.db", PathDefine.ROOT_PATH, null);
                    YugiohUtils.newDatabase(context);
                    if (hProgress != null) {
                        hProgress.sendEmptyMessage(1);
                    }
                }
                if (hProgress != null) {
                    hProgress.sendEmptyMessage(2);
                }
            }
        }).start();

    }
}
