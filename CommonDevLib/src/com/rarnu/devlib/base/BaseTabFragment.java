package com.rarnu.devlib.base;

import android.app.ActionBar;
import android.app.ActionBar.Tab;
import android.app.ActionBar.TabListener;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.support.v4.view.ViewPager;
import android.support.v4.view.ViewPager.OnPageChangeListener;
import com.rarnu.devlib.R;
import com.rarnu.devlib.base.adapter.BaseFragmentAdapter;
import com.rarnu.devlib.base.inner.InnerFragment;

import java.util.ArrayList;
import java.util.List;

public abstract class BaseTabFragment extends InnerFragment implements TabListener, OnPageChangeListener {

    protected ActionBar bar;
    private ViewPager pager;
    private BaseFragmentAdapter adapter;
    private List<Fragment> listFragment;
    private List<String> listTags;
    private int currentPage = 0;
    private boolean needRelease = true;

    public BaseTabFragment(boolean needRelease) {
        super();
        this.needRelease = needRelease;
    }

    public BaseTabFragment() {
        super();
    }

    public BaseTabFragment(String tagText, String tabTitle) {
        super(tagText, tabTitle);
    }

    @Override
    public void onDestroyView() {
        if (needRelease) {
            bar.removeAllTabs();
            bar.setNavigationMode(ActionBar.NAVIGATION_MODE_STANDARD);
            adapter = null;
            listFragment = null;
            listTags = null;
            pager.post(new Runnable() {

                @Override
                public void run() {
                    pager.setAdapter(null);

                }
            });
        }

        super.onDestroyView();
    }

    @Override
    public void initComponents() {
        bar = getActivity().getActionBar();
        bar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);

        pager = (ViewPager) innerView.findViewById(R.id.pager);
        pager.setOffscreenPageLimit(3);
        listFragment = new ArrayList<Fragment>();
        listTags = new ArrayList<String>();
        initFragmentList(listFragment);
        for (Fragment bf : listFragment) {
            listTags.add(((BaseFragment) bf).getTagText());
        }

        adapter = new BaseFragmentAdapter(getFragmentManager(), listFragment, listTags);

        pager.post(new Runnable() {

            @Override
            public void run() {
                pager.setAdapter(adapter);
            }
        });

        initTab();
    }

    public void addTab(final int position, BaseFragment fragment) {

        if (listFragment.indexOf(fragment) == -1) {
            Tab t = bar.newTab().setText(fragment.getTabTitle()).setTabListener(this);
            if (position == -1) {
                listFragment.add(fragment);
                listTags.add(fragment.getTagText());
                bar.addTab(t);
            } else {
                listFragment.add(position, fragment);
                listTags.add(position, fragment.getTagText());
                bar.addTab(t, position);
            }

            adapter = new BaseFragmentAdapter(getFragmentManager(), listFragment, listTags);
            pager.post(new Runnable() {

                @Override
                public void run() {
                    pager.setAdapter(adapter);
                    int newPosition = (position == -1 ? listFragment.size() - 1 : position);
                    pager.setCurrentItem(newPosition);
                }
            });

        }
    }

    public void removeTab(int position) {
        int newPosition = position;
        listFragment.remove(position);
        listTags.remove(position);
        bar.removeTabAt(position);
        newPosition--;
        if (newPosition < 0) {
            newPosition = 0;
        }
        final int nPos = newPosition;
        adapter = new BaseFragmentAdapter(getFragmentManager(), listFragment, listTags);
        pager.post(new Runnable() {

            @Override
            public void run() {
                pager.setAdapter(adapter);
                pager.setCurrentItem(nPos);
            }
        });

    }

    public abstract void initFragmentList(List<Fragment> listFragment);

    private void initTab() {
        bar.removeAllTabs();
        for (int i = 0; i < listFragment.size(); i++) {
            Tab t = bar.newTab().setText(((BaseFragment) listFragment.get(i)).getTabTitle()).setTabListener(this);
            bar.addTab(t);
        }
    }

    @Override
    public void initEvents() {
        pager.setOnPageChangeListener(this);
    }

    @Override
    public void initLogic() {
        pager.post(new Runnable() {

            @Override
            public void run() {
                pager.setCurrentItem(0);

            }
        });
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_tab;
    }

    @Override
    public void onTabReselected(Tab tab, FragmentTransaction ft) {

    }

    public int getCurrentPage() {
        return currentPage;
    }

    @Override
    public void onTabSelected(final Tab tab, FragmentTransaction ft) {
        if (pager.getCurrentItem() != tab.getPosition()) {
            currentPage = tab.getPosition();
            pager.post(new Runnable() {

                @Override
                public void run() {
                    pager.setCurrentItem(tab.getPosition());

                }
            });
        }

    }

    @Override
    public void onTabUnselected(Tab tab, FragmentTransaction ft) {

    }

    @Override
    public void onPageScrollStateChanged(int arg0) {

    }

    @Override
    public void onPageScrolled(int arg0, float arg1, int arg2) {

    }

    @Override
    public void onPageSelected(int position) {
        currentPage = position;
        bar.setSelectedNavigationItem(position);

    }

    public void setTabPosition(final int position) {
        bar.setSelectedNavigationItem(position);
        pager.post(new Runnable() {

            @Override
            public void run() {
                pager.setCurrentItem(position);

            }
        });

    }

}
