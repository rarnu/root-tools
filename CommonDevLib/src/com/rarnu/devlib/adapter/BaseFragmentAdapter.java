package com.rarnu.devlib.adapter;

import android.app.Fragment;
import android.app.FragmentManager;

import java.util.List;

public class BaseFragmentAdapter extends FragmentPagerAdapter {

    private List<Fragment> list;

    public BaseFragmentAdapter(FragmentManager fragmentManager, List<Fragment> listFragment) {
        super(fragmentManager);
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
