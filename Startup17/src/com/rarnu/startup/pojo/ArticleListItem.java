package com.rarnu.startup.pojo;

import android.util.Log;
import org.json.JSONObject;

public class ArticleListItem {
    public long id = 0L;
    public String name = "";
    public String desc = "";
    public String approved_time = "";
    public String image_url = "";
    public String url = "";

    public static ArticleListItem fromJson(JSONObject json) {

        ArticleListItem item = null;
        try {
            item = new ArticleListItem();
            item.id = json.getLong("id");
            item.name = json.getString("name");
            item.url = json.getString("url");
            item.desc = json.getString("desc");
            item.approved_time = json.getString("approved_time");
            item.image_url = json.getString("image_url");
        } catch (Exception e) {
            Log.e("ArticleListItem", e.getMessage());
        }
        return item;
    }
}
