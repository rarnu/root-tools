package com.rarnu.tools.neo.api;

import android.util.Log;
import com.rarnu.tools.neo.data.Onekey;
import com.rarnu.tools.neo.utils.HttpUtils;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

public class API {

    private static final String API_BASE = "http://diy.ourocg.cn/root/";
    public static final String DOWNLOAD_URL = API_BASE + "download/";

    public static String getUpdateInfo() {
        String jsonStr = HttpUtils.get(API_BASE + "version.php", "");
        return jsonStr;
    }

    public static Onekey getOnekey(String pkgName, int versionCode) {
        String str = HttpUtils.get(API_BASE + "onekey.php", String.format("action=get&pkg=%s&ver=%d", pkgName, versionCode));
        Onekey ok = null;
        if (str != null && !str.trim().equals("")) {
            ok = new Onekey(pkgName, str);
        }
        return ok;
    }

    public static boolean uploadOnekey(String pkgName, int versionCode, List<String> disabled) {
        // upload onekey
        Map<String, String> param = new HashMap<>();
        param.put("action", "put");
        param.put("pkg", pkgName);
        param.put("ver", String.valueOf(versionCode));
        String data = "";
        if (disabled != null && disabled.size() != 0) {
            for (String s: disabled) {
                data += s + "\n";
            }
        }
        param.put("data", data);
        String str = HttpUtils.post(API_BASE + "onekey.php", param);
        return str.equals("OK");
    }

    public static boolean sendFeedback(String nickname, String comment, String[] photo) {
        Map<String, String> params = new HashMap<>();
        params.put("nickname", nickname);
        params.put("comment", comment);
        Map<String, String> files = new HashMap<>();
        for (int i = 0; i < photo.length; i++) {
            if (!photo[i].equals("")) {
                files.put(String.format("photo%d", i + 1), photo[i]);
            }
        }
        String data = HttpUtils.postFile(API_BASE + "feedback.php", params, files);
        Log.e("API", "sendFeedback => " + data);
        boolean ret = false;
        try {
            JSONObject json = new JSONObject(data);
            int i = json.getInt("result");
            ret = i == 0;
        } catch (Exception e) {

        }
        return ret;
    }

}
