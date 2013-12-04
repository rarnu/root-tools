package com.rarnu.startup.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseClassLoader;
import com.rarnu.startup.api.MobileApi;
import com.rarnu.startup.pojo.ArticleItem;

public class ArticleLoader extends BaseClassLoader<ArticleItem> {
    private long id;

    public ArticleLoader(Context context) {
        super(context);
    }

    public void setId(long id) {
        this.id = id;
    }

    @Override
    public ArticleItem loadInBackground() {
        return MobileApi.getArticle(id);
    }
}
