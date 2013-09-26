package com.rarnu.devlib.component;

import android.support.v4.view.PagerAdapter;
import android.support.v4.view.ViewPager;
import android.view.View;
import android.view.ViewGroup;

import java.util.List;

public class EndlessPagerAdapter extends PagerAdapter {

    private List<View> views = null;
    private boolean isEndless = false;
    private int count = 0;

    public EndlessPagerAdapter(List<View> views) {
        setNewData(views);
    }

    public void setNewData(List<View> views) {
        this.views = views;
        count = (isEndless ? Integer.MAX_VALUE : this.views.size());
        this.notifyDataSetChanged();
    }

    @Override
    public int getCount() {
        return count;
    }

    public void setEndless(boolean isEndless) {
        this.isEndless = isEndless;
        if (this.views != null) {
            count = (isEndless ? Integer.MAX_VALUE : this.views.size());
        }
    }

    @Override
    public void destroyItem(ViewGroup container, int position, Object object) {

    }

    @Override
    public Object instantiateItem(ViewGroup container, int position) {
        try {
            ((ViewPager) container).addView(views.get(position % views.size()), 0);
        } catch (Exception e) {
        }
        return (views.size() > 0) ? views.get(position % views.size()) : null;
    }

    @Override
    public boolean isViewFromObject(View arg0, Object arg1) {
        return arg0 == arg1;
    }

}
