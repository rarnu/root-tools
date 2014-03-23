package com.rarnu.devlib.base.adapter;

import android.app.Fragment;
import android.app.FragmentManager;

import java.util.List;

public class BaseFragmentStateAdapter extends FragmentStatePagerAdapter {

    private List<Fragment> list;

    public BaseFragmentStateAdapter(FragmentManager fragmentManager, List<Fragment> listFragment) {
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
