package com.rarnu.devlib.base.adapter;

import android.app.Fragment;
import android.app.FragmentManager;

import java.util.List;

public class BaseFragmentAdapter extends FragmentPagerAdapter {

    private List<Fragment> list;

    public BaseFragmentAdapter(FragmentManager fragmentManager, List<Fragment> listFragment, List<String> tags) {
        super(fragmentManager, tags);
        this.list = listFragment;
    }

    @Override
    public Fragment getItem(int position) {
        return list.get(position);
    }

    @Override
    public int getCount() {
        return list.size();
    }

}
