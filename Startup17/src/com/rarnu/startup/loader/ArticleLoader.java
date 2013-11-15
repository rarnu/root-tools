package com.rarnu.startup.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseClassLoader;
import com.rarnu.startup.pojo.ArticleItem;
import com.rarnu.startup.test.TestUtils;

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
        return TestUtils.getTestDetailData(id);
    }
}
