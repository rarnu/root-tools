package com.rarnu.devlib.base;

import android.os.Bundle;
import android.view.LayoutInflater;
import com.rarnu.devlib.component.PullToRefreshExpandableListView;

public class PullToRefreshExpandableListFragment extends PullToRefreshBaseListFragment<PullToRefreshExpandableListView> {

    protected PullToRefreshExpandableListView onCreatePullToRefreshListView(LayoutInflater inflater,
                                                                            Bundle savedInstanceState) {
        return new PullToRefreshExpandableListView(getActivity());
    }
}