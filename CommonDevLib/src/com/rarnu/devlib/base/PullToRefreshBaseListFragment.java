package com.rarnu.devlib.base;

import android.os.Bundle;
import android.support.v4.app.ListFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.ListView;

abstract class PullToRefreshBaseListFragment<T extends PullToRefreshBase<? extends AbsListView>> extends ListFragment {

	private T mPullToRefreshListView;

	@Override
	public final View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		View layout = super.onCreateView(inflater, container, savedInstanceState);

		ListView lv = (ListView) layout.findViewById(android.R.id.list);
		ViewGroup parent = (ViewGroup) lv.getParent();

		int lvIndex = parent.indexOfChild(lv);
		parent.removeViewAt(lvIndex);
		mPullToRefreshListView = onCreatePullToRefreshListView(inflater, savedInstanceState);
		parent.addView(mPullToRefreshListView, lvIndex, lv.getLayoutParams());

		return layout;
	}

	public final T getPullToRefreshListView() {
		return mPullToRefreshListView;
	}

	protected abstract T onCreatePullToRefreshListView(LayoutInflater inflater, Bundle savedInstanceState);

}