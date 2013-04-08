package com.rarnu.devlib.base;

import java.util.ArrayList;
import java.util.List;

import android.app.ActionBar;
import android.app.ActionBar.Tab;
import android.app.ActionBar.TabListener;
import android.app.FragmentTransaction;
import android.support.v4.view.ViewPager;
import android.support.v4.view.ViewPager.OnPageChangeListener;

import com.rarnu.devlib.R;
import com.rarnu.devlib.base.inner.InnerFragment;

public abstract class BaseTabFragment extends InnerFragment implements
		TabListener, OnPageChangeListener {

	protected ActionBar bar;
	private ViewPager pager;
	private BaseFragmentAdapter adapter;
	private List<BaseFragment> listFragment;
	private int currentPage = 0;

	@Override
	protected void initComponents() {
		bar = getActivity().getActionBar();
		bar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);

		pager = (ViewPager) innerView.findViewById(R.id.pager);
		pager.setOffscreenPageLimit(3);
		listFragment = new ArrayList<BaseFragment>();
		initFragmentList(listFragment);
		adapter = new BaseFragmentAdapter(getFragmentManager(), listFragment);
		pager.setAdapter(adapter);
		initTab();
	}

	public void addTab(int position, BaseFragment fragment) {
		
		if (listFragment.indexOf(fragment) == -1) {
			Tab t = bar.newTab().setText(fragment.getTabTitle())
					.setTabListener(this);
			if (position == -1) {
				listFragment.add(fragment);
				bar.addTab(t);
			} else {
				listFragment.add(position, fragment);
				bar.addTab(t, position);
			}
			adapter = new BaseFragmentAdapter(getFragmentManager(), listFragment);
			pager.setAdapter(adapter);
			int newPosition = (position == -1 ? listFragment.size() - 1: position);
			pager.setCurrentItem(newPosition);
		}
	}

	public void removeTab(int position) {
		int newPosition = position;
		listFragment.remove(position);
		bar.removeTabAt(position);
		newPosition--;
		if (newPosition<0) {
			newPosition = 0;
		}
		adapter = new BaseFragmentAdapter(getFragmentManager(), listFragment);
		pager.setAdapter(adapter);
		pager.setCurrentItem(newPosition);

	}

	public abstract void initFragmentList(List<BaseFragment> listFragment);

	private void initTab() {
		bar.removeAllTabs();
		for (int i = 0; i < listFragment.size(); i++) {
			Tab t = bar.newTab().setText(listFragment.get(i).getTabTitle())
					.setTabListener(this);
			bar.addTab(t);
		}
	}

	@Override
	protected void initEvents() {
		pager.setOnPageChangeListener(this);
	}

	@Override
	protected void initLogic() {
		pager.setCurrentItem(0);
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_tab;
	}

	@Override
	public void onTabReselected(Tab tab, FragmentTransaction ft) {

	}
	
	public int getCurrentPage() {
		return currentPage;
	}

	@Override
	public void onTabSelected(Tab tab, FragmentTransaction ft) {
		if (pager.getCurrentItem() != tab.getPosition()) {
			currentPage = tab.getPosition();
			pager.setCurrentItem(tab.getPosition());
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

}
