package com.rarnu.devlib.base;

import java.util.List;

import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.support.v4.view.PagerAdapter;
import android.view.View;
import android.view.ViewGroup;

public class BaseFragmentAdapter extends PagerAdapter {

	private final FragmentManager fragmentManager;
	private FragmentTransaction transaction = null;

	private List<BaseFragment> list;

	public BaseFragmentAdapter(FragmentManager fm, List<BaseFragment> list) {
		fragmentManager = fm;
		this.list = list;

	}

	@Override
	public int getCount() {
		return list.size();
	}
	
	public void setNewData(List<BaseFragment> list) {
		this.list = list;
		notifyDataSetChanged();
	}

	@Override
	public boolean isViewFromObject(View view, Object object) {
		return ((Fragment) object).getView() == view;
	}

	@Override
	public Object instantiateItem(ViewGroup container, int position) {
		if (transaction == null) {
			transaction = fragmentManager.beginTransaction();
		}
		String name = list.get(position).getTagText();
		Fragment fragment = fragmentManager.findFragmentByTag(name);
		if (fragment != null) {
			transaction.attach(fragment);
		} else {
			fragment = list.get(position);
			transaction.add(container.getId(), fragment, name);
		}
		return fragment;
	}

	@Override
	public void destroyItem(ViewGroup container, int position, Object object) {
		if (transaction == null) {
			transaction = fragmentManager.beginTransaction();
		}
		transaction.detach((Fragment) object);
	}

	@Override
	public void finishUpdate(ViewGroup container) {
		if (transaction != null) {
			transaction.commitAllowingStateLoss();
			transaction = null;
			// fragmentManager.executePendingTransactions();
		}
	}

}
