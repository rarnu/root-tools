package com.rarnu.devlib.component;

import android.content.Context;
import android.support.v4.view.ViewPager;
import android.util.AttributeSet;
import android.view.View;
import com.rarnu.devlib.component.intf.OnPageSelected;

import java.util.ArrayList;
import java.util.List;

public class EndlessViewPager extends ViewPager {

    private EndlessPagerAdapter adapter;
    private List<View> views;
    private int curPosition = 0;
    private int maxPage = Integer.MAX_VALUE;
    private OnPageSelected pageSelected;

    public EndlessViewPager(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public EndlessViewPager(Context context) {
        super(context);
        init();
    }

    public void setOnPageSelected(OnPageSelected pageSelected) {
        this.pageSelected = pageSelected;
    }

    private void init() {
        views = new ArrayList<View>();
        adapter = new EndlessPagerAdapter(views);
        setAdapter(adapter);
        setOnPageChangeListener(new OnPageChangeListener() {

            @Override
            public void onPageSelected(int position) {
                curPosition = position;
                if (pageSelected != null) {
                    pageSelected.onPageSelected(position % views.size());
                }
            }

            @Override
            public void onPageScrolled(int arg0, float arg1, int arg2) {

            }

            @Override
            public void onPageScrollStateChanged(int arg0) {

            }
        });
    }

    public void setData(List<View> views) {
        this.views = views;
        adapter.setNewData(views);
    }

    public void setEndless(boolean isEndless) {
        adapter.setEndless(isEndless);
    }

    public void scrollLeft() {

        setCurrentItem((--curPosition <= 0) ? maxPage : curPosition);
    }

    public void scrollRight() {
        setCurrentItem((++curPosition >= maxPage) ? 0 : curPosition);
    }

}
