package com.yugioh.android.database;

import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class YugiohUtils {

    public static void closeDatabase(Context context) {
        context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_CLOSEDATABASE), null, null, null, null);
    }

    public static void newDatabase(Context context) {
        context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_NEWDATABASE), null, null, null, null);
    }

    public static CardInfo getOneCard(Context context, int cardId) {
        Cursor cursor = context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, cardId), null, null, null, null);
        CardInfo info = null;
        if (cursor != null) {
            cursor.moveToFirst();
            while (!cursor.isAfterLast()) {
                try {
                    info = cursorToCardInfo(cursor);
                } catch (Exception e) {
                }
                break;
            }
            cursor.close();
        }
        return info;
    }

    public static int getLastCardId(Context context) {
        Cursor cursor = context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_CARDCOUNT), null, null, null, null);
        int count = 0;
        if (cursor != null) {
            cursor.moveToFirst();
            while (!cursor.isAfterLast()) {
                count = cursor.getInt(0);
                break;
            }
            cursor.close();
        }
        return count;
    }

    public static int getDatabaseVersion(Context context) {
        Cursor c = context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_VERSION), null, null, null, null);
        int ver = 1;
        if (c != null) {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                ver = c.getInt(c.getColumnIndex("ver"));
                c.moveToNext();
            }
            c.close();
        }
        return ver;
    }

    public static Cursor getLatest100(Context context) {
        return context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_TOP100), null, null, null, null);
    }

    public static Cursor getCardNames(Context context, String cardName) {
        String where = "name like ? or oldName like ? or shortName like ?";
        String argStr = "%" + cardName + "%";
        String[] args = new String[]{argStr, argStr, argStr};
        return context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_SEARCH), new String[]{"_id", "name"}, where, args, null);
    }

    public static Cursor getCards(Context context, String cardType, String cardAttribute, int cardLevel, String cardRace, String cardName, String cardEffect, String cardAtk, String cardDef, String cardRare, String cardBelongs, String cardLimit, String cardTunner, String cardEffectText) {

        int argCnt = 0;
        String where = "1=1";
        if (!cardType.equals("")) {
            where += " and sCardType like ?";
            argCnt++;
        }
        if (!cardTunner.equals("")) {
            if (cardType.contains(context.getString(R.string.monster))) {
                where += " and CardDType like ?";
                argCnt++;
            }
        }
        if (!cardAttribute.equals("")) {
            where += " and element=?";
            argCnt++;
        }
        if (!cardRace.equals("")) {
            where += " and tribe=?";
            argCnt++;
        }
        if (cardLevel != 0) {
            where += " and level=?";
            argCnt++;
        }
        if (!cardName.equals("")) {
            where += " and (name like ? or oldName like ? or shortName like ?)";
            argCnt += 3;

        }
        if (!cardRare.equals("")) {
            where += " and infrequence like ?";
            argCnt++;
        }
        if (!cardBelongs.equals("")) {
            where += " and cardCamp=?";
            argCnt++;
        }
        if (!cardAtk.equals("")) {
            if (isNumeric(cardAtk)) {
                where += " and atkValue=?";
            } else {
                where += " and atk=?";
            }
            argCnt++;
        }
        if (!cardDef.equals("")) {
            if (isNumeric(cardDef)) {
                where += " and defValue=?";
            } else {
                where += " and def=?";
            }
            argCnt++;
        }
        if (!cardLimit.equals("")) {
            where += " and ban=?";
            argCnt++;
        }
        if (!cardEffectText.equals("")) {
            where += " and effect like ?";
            argCnt++;
        }

        String[] args = new String[argCnt];
        int argId = 0;
        if (!cardType.equals("")) {
            args[argId] = "%" + cardType + "%";
            argId++;
        }
        if (!cardTunner.equals("")) {
            if (cardType.contains(context.getString(R.string.monster))) {
                args[argId] = "%" + cardTunner + "%";
                argId++;
            }
        }
        if (!cardAttribute.equals("")) {
            args[argId] = cardAttribute;
            argId++;
        }
        if (!cardRace.equals("")) {
            args[argId] = cardRace;
            argId++;
        }
        if (cardLevel != 0) {
            args[argId] = String.valueOf(cardLevel);
            argId++;
        }
        if (!cardName.equals("")) {
            args[argId] = "%" + cardName + "%";
            argId++;
            args[argId] = "%" + cardName + "%";
            argId++;
            args[argId] = "%" + cardName + "%";
            argId++;

        }

        if (!cardRare.equals("")) {
            args[argId] = "%" + cardRare + "%";
            argId++;
        }

        if (!cardBelongs.equals("")) {
            args[argId] = cardBelongs;
            argId++;
        }

        if (!cardAtk.equals("")) {
            args[argId] = cardAtk;
            argId++;
        }
        if (!cardDef.equals("")) {
            args[argId] = cardDef;
            argId++;
        }
        if (!cardLimit.equals("")) {
            args[argId] = cardLimit;
            argId++;
        }

        if (!cardEffectText.equals("")) {
            args[argId] = "%" + cardEffectText + "%";
            argId++;
        }
        String LogArg = "";
        for (String l : args) {
            LogArg += l + ", ";
        }
        return context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_SEARCH), new String[]{"_id", "name", "sCardType"}, where, args, null);

    }

    public static Cursor getBannedCards(Context context) {

        String where = "ban=?";
        String args[] = new String[]{context.getResources().getString(R.string.card_banned_pure)};
        String sort = "sCardType asc";

        return context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_SEARCH), new String[]{"_id", "name", "sCardType"}, where, args, sort);

    }

    public static Cursor getLimit1Cards(Context context) {
        String where = "ban=?";
        String args[] = new String[]{context.getResources().getString(R.string.card_limit1_pure)};
        String sort = "sCardType asc";

        return context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_SEARCH), new String[]{"_id", "name", "sCardType"}, where, args, sort);
    }

    public static Cursor getLimit2Cards(Context context) {
        String where = "ban=?";
        String args[] = new String[]{context.getResources().getString(R.string.card_limit2_pure)};
        String sort = "sCardType asc";

        return context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_SEARCH), new String[]{"_id", "name", "sCardType"}, where, args, sort);
    }

    public static Cursor getAssignedCards(Context context, String union) {
        String where = "_id in (" + union + ")";
        return context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_SEARCH), new String[]{"_id", "name", "sCardType"}, where, null, null);
    }

    public static Cursor getCardsViaIds(Context context, int[] ids) {
        String idList = "";
        for (int i = 0; i < ids.length; i++) {
            idList += String.format("%d,", ids[i]);
        }
        idList = idList.substring(0, idList.length() - 1);
        String where = "_id in (" + idList + ")";
        return context.getContentResolver().query(ContentUris.withAppendedId(YugiohProvider.CONTENT_URI, YugiohProvider.ACTIONID_SEARCH), new String[]{"_id", "name", "sCardType"}, where, null, null);
    }

    public static CardInfo cursorToCardInfo(Cursor c) throws Exception {

        CardInfo info = new CardInfo();
        Class<?> cls = info.getClass();
        Field[] fs = cls.getDeclaredFields();
        Method mSet = null;
        String fName = "";
        String fDBName = "";
        try {
            info.setId(c.getInt(c.getColumnIndex("_id")));
            for (Field f : fs) {
                if (f.getName().equals("_id")) {
                    continue;
                }
                fDBName = f.getName().replace("_", "");
                fName = fDBName.substring(0, 1).toUpperCase() + fDBName.substring(1);
                mSet = cls.getMethod("set" + fName, new Class[]{f.getType()});
                if (f.getType().getName().contains("String")) {
                    mSet.invoke(info, new Object[]{c.getString(c.getColumnIndex(fDBName))});
                } else {
                    mSet.invoke(info, new Object[]{c.getInt(c.getColumnIndex(fDBName))});
                }
            }

        } catch (Exception e) {

        }

        return info;
    }

    private static boolean isNumeric(String str) {
        if (str.matches("\\d*")) {
            return true;
        } else {
            return false;
        }
    }
}
