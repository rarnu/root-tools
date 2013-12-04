package com.rarnu.startup.api;


import com.rarnu.startup.pojo.ArticleItem;
import com.rarnu.startup.pojo.ArticleListItem;
import com.rarnu.utils.HttpRequest;
import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

public class MobileApi {

    private static final String API_KEY = "BPQ##17STARTUP##NB2013";
    private static final String HOST = "http://17startup.com/apis/";
    private static final String HOST_GET_LIST = HOST + "get_list";
    private static final String HOST_GET_INFO = HOST + "get_info";
    private static final String PARAM_GET_LIST = "category=%d&page=%d&page_size=%d&verify=%s";
    private static final String PARAM_GET_INFO = "id=%d&verify=%s";

    public static List<ArticleListItem> getArticleList(int categoryId, int page, int pageSize) {
        List<ArticleListItem> list = null;
        String verify = null;

        try {
            verify = md5(String.format("%d%d%d%s", categoryId, page, pageSize, API_KEY));
        } catch (Exception e) {
        }
        if (verify != null) {
            String ret = HttpRequest.get(HOST_GET_LIST, String.format(PARAM_GET_LIST, categoryId, page, pageSize, verify), HTTP.UTF_8);

            try {
                JSONObject json = new JSONObject(ret);
                if (json.getInt("status") == 0) {
                    list = new ArrayList<ArticleListItem>();
                    JSONArray jarr = json.getJSONArray("content");
                    for (int i = 0; i < jarr.length(); i++) {
                        ArticleListItem item = ArticleListItem.fromJson(jarr.getJSONObject(i));

                        if (item != null) {
                            list.add(item);
                        }
                    }
                }
            } catch (Exception e) {

            }
        }
        return list;
    }

    public static ArticleItem getArticle(long id) {
        ArticleItem item = null;
        String verify = null;
        try {
            verify = md5(String.format("%d%s", id, API_KEY));
        } catch (Exception e) {

        }
        if (verify != null) {
            String ret = HttpRequest.get(HOST_GET_INFO, String.format(PARAM_GET_INFO, id, verify), HTTP.UTF_8);
            try {
                JSONObject json = new JSONObject(ret);
                if (json.getInt("status") == 0) {
                    item = ArticleItem.fromJson(json.getJSONObject("content"));
                }
            } catch (Exception e) {

            }
        }
        return item;
    }

    private static String md5(String input) throws NoSuchAlgorithmException {
        String result = null;
        if (input != null) {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.update(input.getBytes());
            BigInteger hash = new BigInteger(1, md.digest());
            result = hash.toString(16);
            while (result.length() < 32) {
                result = "0" + result;
            }
        }
        return result;
    }
}
