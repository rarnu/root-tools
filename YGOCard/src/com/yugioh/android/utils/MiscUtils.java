package com.yugioh.android.utils;

import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.os.Handler;
import android.os.Message;
import com.rarnu.utils.FileUtils;
import com.yugioh.android.CardInfoActivity;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.classes.CardItems;
import com.yugioh.android.database.YugiohUtils;
import com.yugioh.android.define.PathDefine;

public class MiscUtils {

    public static void openCardDetail(Context context, Cursor c, int position) {
        c.moveToPosition(position);
        int cardId = c.getInt(0);

        Intent inCardInfo = new Intent(context, CardInfoActivity.class);
        CardInfo info = YugiohUtils.getOneCard(context, cardId);
        inCardInfo.putExtra("cardinfo", info);
        context.startActivity(inCardInfo);

    }

    /**
     * @param type 0:pack, 1:deck
     * @param id
     * @param h
     */
    public static void loadCardsDataT(final int type, final String id, final Handler h, final boolean refresh) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                Message msg = new Message();
                msg.what = 1;
                CardItems items = null;
                switch (type) {
                    case 0:
                        if (refresh) {
                            items = YGOAPI.getPackageCards(id);
                            if (items != null) {
                                FileUtils.saveObjectToFile(items, String.format(PathDefine.PACK_ITEM, id));
                            } else {
                                items = (CardItems) FileUtils.loadObjetFromFile(String.format(PathDefine.PACK_ITEM, id));
                            }
                        } else {
                            items = (CardItems) FileUtils.loadObjetFromFile(String.format(PathDefine.PACK_ITEM, id));
                            if (items == null) {
                                items = YGOAPI.getPackageCards(id);
                                if (items != null) {
                                    FileUtils.saveObjectToFile(items, String.format(PathDefine.PACK_ITEM, id));
                                }
                            }
                        }
                        break;
                    case 1:
                        if (refresh) {
                            items = YGOAPI.getDeckCards(id);
                            if (items != null) {
                                FileUtils.saveObjectToFile(items, String.format(PathDefine.DECK_ITEM, id));
                            } else {
                                items = (CardItems) FileUtils.loadObjetFromFile(String.format(PathDefine.DECK_ITEM, id));
                            }
                        } else {
                            items = (CardItems) FileUtils.loadObjetFromFile(String.format(PathDefine.DECK_PATH, id));
                            if (items == null) {
                                items = YGOAPI.getDeckCards(id);
                                if (items != null) {
                                    FileUtils.saveObjectToFile(items, String.format(PathDefine.DECK_ITEM, id));
                                }
                            }
                        }
                        break;
                }
                if (items != null) {
                    items.id = id;
                }
                msg.obj = items;
                h.sendMessage(msg);
            }
        }).start();

    }
}
