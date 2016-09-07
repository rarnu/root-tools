package com.rarnu.tools.neo.utils;

import android.util.Log;
import com.squareup.okhttp.*;

import java.util.Iterator;
import java.util.Map;

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

    public static String post(String host, Map<String, String> param) {
        OkHttpClient http = new OkHttpClient();
        FormEncodingBuilder builder = new FormEncodingBuilder();
        Iterator<String> iter = param.keySet().iterator();
        while (iter.hasNext()) {
            String key = iter.next();
            String value = param.get(key);
            builder.add(key, value);
        }
        RequestBody body = builder.build();
        Request req = new Request.Builder().url(host).post(body).build();
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
