package com.rarnu.tools.neo.api;

import android.util.Log;
import com.rarnu.tools.neo.data.Onekey;
import com.rarnu.tools.neo.utils.HttpUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

}
