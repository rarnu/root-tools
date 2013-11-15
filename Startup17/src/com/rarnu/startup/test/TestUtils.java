package com.rarnu.startup.test;

import com.rarnu.startup.pojo.ArticleItem;
import com.rarnu.startup.pojo.ArticleListItem;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class TestUtils {

    public static List<ArticleListItem> getTestData() {

        List<ArticleListItem> list = new ArrayList<ArticleListItem>();
        for (int i=0; i<50; i++) {
            ArticleListItem item = new ArticleListItem();
            item.id = i;
            item.title = String.format("Test Item %d", i+1);
            item.desc = String.format("Test Item Desc %d", i+1);
            item.datetime = new SimpleDateFormat("yyyy-MM-dd hh:mm").format(new Date());
            item.readed = false;
            item.image = "";
            list.add(item);
        }

        return list;
    }

    public static ArticleItem getTestDetailData(long id) {
        ArticleItem item = new ArticleItem();
        item.id = id;
        item.title = String.format("Test Item %d", id+1);
        item.desc = String.format("Test Item Desc %d", id+1);
        item.datetime =   new SimpleDateFormat("yyyy-MM-dd hh:mm").format(new Date());
        item.author = "rarnu";
        item.html = "";
        return item;
    }
}
