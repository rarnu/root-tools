package com.yugioh.android.utils;

import android.content.Context;
import android.os.Build;
import com.rarnu.utils.DeviceUtils;
import com.rarnu.utils.HttpRequest;
import com.yugioh.android.classes.*;
import com.yugioh.android.define.NetworkDefine;
import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

public class YGOAPI {

    public static UpdateInfo findUpdate(Context context, int dbVer, int lastCardId) {
        String param = String.format(NetworkDefine.UPDATE_PARAM_FMT, DeviceUtils.getAppVersionCode(context), lastCardId, dbVer);
        UpdateInfo ui = null;
        try {
            String jsonstr = HttpRequest.get(NetworkDefine.UPDATE_URL, param, HTTP.UTF_8);
            JSONObject json = new JSONObject(jsonstr);
            ui = new UpdateInfo();
            ui.setUpdateApk(json.getInt("apk"));
            ui.setUpdateData(json.getInt("data"));
            ui.setNewCard(json.getInt("newcard"));
            ui.setApkVersion(json.getString("apkversion"));
        } catch (Exception e) {

        }
        return ui;

    }

    public static List<RecommandInfo> getRecommands() {
        List<RecommandInfo> list = null;
        try {
            String ret = HttpRequest.get(NetworkDefine.RECOMMAND_URL, "", HTTP.UTF_8);
            JSONObject json = new JSONObject(ret);
            JSONArray jarr = json.getJSONArray("data");
            list = new ArrayList<RecommandInfo>();
            for (int i = 0; i < jarr.length(); i++) {
                RecommandInfo item = new RecommandInfo();
                item.id = jarr.getJSONObject(i).getInt("id");
                item.name = jarr.getJSONObject(i).getString("name");
                item.jumpMode = jarr.getJSONObject(i).getInt("jump_mode");
                item.jumpUrl = jarr.getJSONObject(i).getString("jump_url");
                item.jumpText = jarr.getJSONObject(i).getString("jump_text");
                item.imagePath = jarr.getJSONObject(i).getString("image_name");
                item.bigQR = jarr.getJSONObject(i).getString("big_qr");
                list.add(item);
            }
        } catch (Exception e) {

        }
        return list;
    }

    public static List<PackageItem> getPackageList() {
        List<PackageItem> list = null;
        try {
            String ret = HttpRequest.get(NetworkDefine.URL_OCGSOFT_GET_PACKAGE, "", HTTP.UTF_8);
            JSONArray jarr = new JSONArray(ret);

            list = new ArrayList<PackageItem>();
            JSONObject jobj = null;
            JSONArray jarrPkg = null;
            for (int i = 0; i < jarr.length(); i++) {
                jobj = jarr.getJSONObject(i);
                list.add(new PackageItem(true, "", jobj.getString("serial")));
                jarrPkg = jobj.getJSONArray("packages");
                for (int j = 0; j < jarrPkg.length(); j++) {
                    list.add(new PackageItem(false, jarrPkg.getJSONObject(j).getString("id"), jarrPkg.getJSONObject(j).getString("packname")));
                }
            }
        } catch (Exception e) {

        }
        return list;
    }

    public static CardItems getPackageCards(String id) {
        CardItems item = null;
        try {
            String ret = HttpRequest.get(String.format(NetworkDefine.URL_OCGSOFT_GET_PACKAGE_CARD, id), "", HTTP.UTF_8);
            JSONObject json = new JSONObject(ret);
            item = new CardItems();
            item.packageName = json.getString("name");
            JSONArray jarr = json.getJSONArray("cards");
            item.cardIds = new int[jarr.length()];
            for (int i = 0; i < jarr.length(); i++) {
                item.cardIds[i] = jarr.getInt(i);
            }
        } catch (Exception e) {

        }
        return item;
    }

    public static List<DeckItem> getDeckList() {
        // TODO: fake method
        List<DeckItem> list = new ArrayList<DeckItem>();
        list.add(new DeckItem("1", "DEMO1", "DEMO"));
        list.add(new DeckItem("2", "DEMO2", "DEMO"));
        list.add(new DeckItem("3", "DEMO3", "DEMO"));

        return list;
    }

    public static CardItems getDeckCards(String id) {
        // TODO: fake method
        CardItems items = new CardItems();
        items.packageName = id;
        items.cardIds = new int[40];
        for (int i = 0; i < 40; i++) {
            items.cardIds[i] = i + 50;
        }
        return items;
    }

    public static boolean sendFeedback(Context context, String text) {
        boolean ret = false;
        try {
            String deviceId = DeviceUtils.getDeviceUniqueId(context);
            String email = URLEncoder.encode(AccountUtils.getBindedEmailAddress(context), HTTP.UTF_8);
            text = URLEncoder.encode(text, HTTP.UTF_8);
            int appver = DeviceUtils.getAppVersionCode(context);
            int osver = Build.VERSION.SDK_INT;
            String str = HttpRequest.get(NetworkDefine.FEEDBACK_URL, String.format(NetworkDefine.FEEDBACK_PARAM_FMT, deviceId, email, text, appver, osver), HTTP.UTF_8);
            ret = !str.equals("0");
        } catch (Exception e) {

        }
        return ret;
    }

    public static String getUpdateLog() {
        String ret = "";
        try {
            ret = HttpRequest.get(NetworkDefine.URL_UPDATE_LOG, "", HTTP.UTF_8);
        } catch (Exception e) {

        }
        return ret;
    }
}
