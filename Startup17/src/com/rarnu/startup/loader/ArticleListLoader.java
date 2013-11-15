package com.rarnu.startup.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.startup.pojo.ArticleListItem;
import com.rarnu.startup.test.TestUtils;

import java.util.List;

public class ArticleListLoader extends BaseLoader<ArticleListItem> {
    public ArticleListLoader(Context context) {
        super(context);
    }

    @Override
    public List<ArticleListItem> loadInBackground() {
        return TestUtils.getTestData();
    }
}
