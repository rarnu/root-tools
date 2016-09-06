package com.rarnu.tools.neo.utils;

import com.squareup.okhttp.Call;
import com.squareup.okhttp.OkHttpClient;
import com.squareup.okhttp.Request;
import com.squareup.okhttp.Response;

public class HttpUtils {

    public static String get(String host, String param) {
        OkHttpClient http = new OkHttpClient();
        Request req = new Request.Builder().url(host + "?" + param).build();
        Call call = http.newCall(req);
        String ret = "";
        try {
            Response resp = call.execute();
            if (resp.isSuccessful()) {
                ret = resp.body().string();
            }
        } catch (Exception e) {

        }
        return ret;
    }

}
