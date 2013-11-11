package com.rarnu.startup.fragment;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;
import com.rarnu.devlib.base.BaseTabFragment;
import com.rarnu.startup.R;

import java.util.List;

public class MainFragment extends BaseTabFragment {

    int[] types = new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8};
    ArticleListFragment[] lfs = new ArticleListFragment[types.length];


    @Override
    public void initFragmentList(List<Fragment> listFragment) {

        for (int i=0; i<types.length; i++) {
            lfs[i] = new ArticleListFragment();
            lfs[i].setType(types[i]);
            listFragment.add(lfs[i]);
        }
    }

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
    public String getMainActivityName() {
        return null;
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
