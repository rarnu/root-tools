package com.rarnu.startup.fragment;

import android.os.Bundle;
import android.view.Menu;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.startup.R;
import com.rarnu.utils.ResourceUtils;

public class ArticleListFragment extends BaseFragment {

    private int type = 0;
    private int[] tags = new int[]{
            R.string.tag_0, R.string.tag_1, R.string.tag_2,
            R.string.tag_3, R.string.tag_4, R.string.tag_5,
            R.string.tag_6, R.string.tag_7, R.string.tag_8
    };
    private int[] titles = new int[]{
            R.string.title_0, R.string.title_1, R.string.title_2,
            R.string.title_3, R.string.title_4, R.string.title_5,
            R.string.title_6, R.string.title_7, R.string.title_8
    };

    @Override
    public int getBarTitle() {
        return R.string.app_name;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.app_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {

    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.main;
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

    public void setType(int type) {
        this.type = type;
        tagText = ResourceUtils.getString(tags[type]);
        tabTitle = ResourceUtils.getString(titles[type]);
    }
}
