package com.yugioh.android.utils;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.database.YugiohUtils;

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
}
