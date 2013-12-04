package com.rarnu.startup.pojo;

import org.json.JSONArray;
import org.json.JSONObject;

public class ArticleItem {
    public long id = 0L;
    public String name = "";
    public String desc = "";
    public long user_id = 0L;
    public String user_name = "";
    public String approved_time = "";
    public String image_url = "";
    public String url = "";
    public String[] tag_info = null;

    public static ArticleItem fromJson(JSONObject json) {
        ArticleItem item = null;
        try {
            item = new ArticleItem();
            item.id = json.getLong("id");
            item.name = json.getString("name");
            item.url = json.getString("url");
            item.desc = json.getString("desc");
            item.user_id = json.getLong("user_id");
            item.user_name = json.getString("user_name");
            item.approved_time = json.getString("approved_time");
            item.image_url = json.getString("image_url");
            JSONArray jarrTag = json.getJSONArray("tag_info");
            item.tag_info = new String[jarrTag.length()];
            for (int i = 0; i < jarrTag.length(); i++) {
                item.tag_info[i] = jarrTag.getString(i);
            }
        } catch (Exception e) {

        }

        return item;
    }
}
