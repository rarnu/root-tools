package com.rarnu.tools.neo.utils;

import okhttp3.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;

public class HttpUtils {

    public static class HttpRequestResponseData {

        public CookieJar cookie;
        public String data;
    }


    public static String simplePostWithHeader(String host, String param, String encoding, Map<String, String> property) throws Exception {
        URL url = new URL(host);
        URLConnection conn = url.openConnection();
        if (property != null) {
            Iterator<Entry<String, String>> iter = property.entrySet().iterator();
            while (iter.hasNext()) {
                Map.Entry<String, String> entry = iter.next();
                conn.addRequestProperty(entry.getKey(), entry.getValue());
            }
        }
        conn.setDoOutput(true);
        OutputStreamWriter osw = new OutputStreamWriter(conn.getOutputStream());
        osw.write(param);
        osw.flush();
        osw.close();
        InputStreamReader isr = new InputStreamReader(conn.getInputStream(), encoding);
        BufferedReader br = new BufferedReader(isr);
        String content = null;
        String result = "";
        while ((content = br.readLine()) != null) {
            result += content + "\n";
        }
        br.close();
        isr.close();
        return result;
    }

    public static String simplePost(String host, String param, String encoding) throws Exception {
        return simplePostWithHeader(host, param, encoding, null);
    }

    public static String post(String host, String getParams, Map<String, String> params) {
        String url = host + "?" + getParams;
        return post(url, params);
    }

    public static String post(String host, Map<String, String> params) {
        RequestBody body = buildBody(params);
        Request req = new Request.Builder().url(host).post(body).build();
        return executeForResult(req);
    }

    public static String postFile(String host, Map<String, String> params, Map<String, String> files) {
        RequestBody body = buildPostFileParts(params, files);
        Request req = new Request.Builder().url(host).post(body).build();
        return executeForResult(req);
    }

    public static HttpRequestResponseData postWithCookie(String host, Map<String, String> params, CookieJar cookie) {
        RequestBody body = buildBody(params);
        Request req = new Request.Builder().url(host).post(body).build();
        return executeForData(req, cookie);
    }

    public static HttpRequestResponseData postFileWithCookie(String host, Map<String, String> params, Map<String, String> files, CookieJar cookie) {
        RequestBody body = buildPostFileParts(params, files);
        Request req = new Request.Builder().url(host).post(body).build();
        return executeForData(req, cookie);
    }

    private static RequestBody buildPostFileParts(Map<String, String> params, Map<String, String> files) {
        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);
        Iterator<String> iterParam = params.keySet().iterator();
        while (iterParam.hasNext()) {
            String key = iterParam.next();
            String val = params.get(key);
            builder.addFormDataPart(key, val);
        }

        Iterator<String> iterFile = files.keySet().iterator();
        while (iterFile.hasNext()) {
            String key = iterFile.next();
            String file = files.get(key);
            File f = new File(file);
            RequestBody fileBody = RequestBody.create(MediaType.parse("application/octet-stream"), f);
            builder.addFormDataPart(key, file.substring(file.lastIndexOf("/") + 1), fileBody);
        }

        return builder.build();
    }

    public static String get(String host, String params) {
        Request req = new Request.Builder().url(host + "?" + params).build();
        return executeForResult(req);
    }

    public static String delete(String host, Map<String, String> params) {
        RequestBody body = buildBody(params);
        Request req = new Request.Builder().url(host).delete(body).build();
        return executeForResult(req);
    }

    public static HttpRequestResponseData getWithCookie(String host, String params, CookieJar cookie) {
        Request req = new Request.Builder().url(host + "?" + params).build();
        return executeForData(req, cookie);
    }

   private static RequestBody buildBody(Map<String, String> params) {
        FormBody.Builder builder = new FormBody.Builder();
        Iterator<String> iter = params.keySet().iterator();
        while (iter.hasNext()) {
            String key = iter.next();
            String value = params.get(key);
            builder.add(key, value);
        }
        return builder.build();
    }

    private static String executeForResult(Request req) {
        OkHttpClient http = new OkHttpClient.Builder().connectTimeout(10, TimeUnit.SECONDS).build();
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

    private static HttpRequestResponseData executeForData(Request req, CookieJar cookie) {
        HttpRequestResponseData data = null;
        OkHttpClient.Builder builder = new OkHttpClient.Builder();
        builder.connectTimeout(10, TimeUnit.SECONDS);
        if (cookie != null) {
            builder.cookieJar(cookie);
        }
        OkHttpClient http = builder.build();
        Call call = http.newCall(req);
        String ret = "";
        try {
            Response resp = call.execute();
            if (resp.isSuccessful()) {
                data = new HttpRequestResponseData();
                data.data = resp.body().string();;
                data.cookie = http.cookieJar();
            }
        } catch (Exception e) {

        }
        return data;
    }

}
