package com.rarnu.devlib.base;

import java.util.List;

import com.rarnu.devlib.base.inner.FragmentPagerAdapter;

import android.app.Fragment;
import android.app.FragmentManager;

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
