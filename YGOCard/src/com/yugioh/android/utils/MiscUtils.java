package com.yugioh.android.utils;

import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.os.Handler;
import android.os.Message;
import com.yugioh.android.AboutActivity;
import com.yugioh.android.CardInfoActivity;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.database.YugiohUtils;

public class MiscUtils {

    public static void openCardDetail(Context context, Cursor c, int position) {
        c.moveToPosition(position);
        int cardId = c.getInt(0);
        if (cardId == 1) {
            context.startActivity(new Intent(context, AboutActivity.class));
        } else {
            Intent inCardInfo = new Intent(context, CardInfoActivity.class);
            CardInfo info = YugiohUtils.getOneCard(context, cardId);
            inCardInfo.putExtra("cardinfo", info);
            context.startActivity(inCardInfo);
        }
    }

    /**
     * @param type 0:pack, 1:deck
     * @param id
     * @param h
     */
    public static void loadCardsDataT(final int type, final String id, final Handler h) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                Message msg = new Message();
                msg.what = 1;
                switch (type) {
                    case 0:
                        msg.obj = YGOAPI.getPackageCards(id);
                        break;
                    case 1:
                        msg.obj = YGOAPI.getDeckCards(id);
                        break;
                }

                h.sendMessage(msg);
            }
        }).start();

    }
}
