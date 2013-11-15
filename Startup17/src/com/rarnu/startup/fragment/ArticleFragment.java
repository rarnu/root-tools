package com.rarnu.startup.fragment;

import android.os.Bundle;
import android.view.Menu;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.startup.R;
import com.rarnu.startup.loader.ArticleLoader;

public class ArticleFragment extends BaseFragment {

    ArticleLoader loader;

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void initEvents() {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void initLogic() {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_article;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }
}
