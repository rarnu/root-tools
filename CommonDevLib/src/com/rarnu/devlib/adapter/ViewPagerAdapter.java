package com.rarnu.devlib.adapter;

import android.support.v4.view.PagerAdapter;
import android.support.v4.view.ViewPager;
import android.view.View;
import android.view.ViewGroup;

import java.util.List;

public class ViewPagerAdapter extends PagerAdapter {

    private List<View> views = null;

    public ViewPagerAdapter(List<View> views) {
        setNewData(views);
    }

    public void setNewData(List<View> views) {
        this.views = views;
        this.notifyDataSetChanged();
    }

    @Override
    public int getCount() {
        return this.views.size();
    }

    @Override
    public void destroyItem(ViewGroup container, int position, Object object) {

    }

    @Override
    public Object instantiateItem(ViewGroup container, int position) {
        try {
            ((ViewPager) container).addView(views.get(position), 0);
        } catch (Exception e) {
        }
        return (views.size() > 0) ? views.get(position) : null;
    }

    @Override
    public boolean isViewFromObject(View arg0, Object arg1) {
        return arg0 == arg1;
    }

}
