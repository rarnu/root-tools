package com.rarnu.tools.neo.api;

import com.rarnu.tools.neo.utils.HttpUtils;

public class API {

    private static final String API_BASE = "http://diy.ourocg.cn/root/";
    public static final String DOWNLOAD_URL = API_BASE + "download/";

    public static String getUpdateInfo() {
        String jsonStr = HttpUtils.get(API_BASE + "version.php", "");
        return jsonStr;
    }

}
