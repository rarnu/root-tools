package com.rarnu.startup.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.startup.api.MobileApi;
import com.rarnu.startup.pojo.ArticleListItem;

import java.util.List;

public class ArticleListLoader extends BaseLoader<ArticleListItem> {

    private int categoryId;
    private int page;
    private int pageSize;

    public ArticleListLoader(Context context) {
        super(context);
    }

    public void setData(int categoryId, int page, int pageSize) {
        this.categoryId = categoryId;
        this.page = page;
        this.pageSize = pageSize;
    }

    @Override
    public List<ArticleListItem> loadInBackground() {
        return MobileApi.getArticleList(categoryId, page, pageSize);
    }
}
