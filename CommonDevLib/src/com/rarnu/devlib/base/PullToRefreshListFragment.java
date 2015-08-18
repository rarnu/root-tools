package com.rarnu.devlib.base;

import android.os.Bundle;
import android.view.LayoutInflater;
import com.rarnu.devlib.component.PullToRefreshListView;

public class PullToRefreshListFragment extends PullToRefreshBaseListFragment<PullToRefreshListView> {

	protected PullToRefreshListView onCreatePullToRefreshListView(LayoutInflater inflater, Bundle savedInstanceState) {
		return new PullToRefreshListView(getActivity());
	}

}