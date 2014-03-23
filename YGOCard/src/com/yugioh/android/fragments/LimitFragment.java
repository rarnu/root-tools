package com.yugioh.android.fragments;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;
import com.rarnu.devlib.base.BaseTabFragment;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.R;

import java.util.List;

public class LimitFragment extends BaseTabFragment {

    public LimitFragment() {
        super();
    }

    @Override
    public void initFragmentList(List<Fragment> listFragment) {
        listFragment.add(new LimitDetailFragment0());
        listFragment.add(new LimitDetailFragment1());
        listFragment.add(new LimitDetailFragment2());
    }

    @Override
    public int getBarTitle() {
        return R.string.lm_banned;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.lm_banned;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu arg0) {

    }

    @Override
    public void onGetNewArguments(Bundle arg0) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
