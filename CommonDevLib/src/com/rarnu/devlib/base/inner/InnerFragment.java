package com.rarnu.devlib.base.inner;

import android.app.Fragment;
import android.os.Bundle;
import android.view.*;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import com.rarnu.devlib.base.intf.InnerIntf;
import com.rarnu.devlib.common.UIInstance;

public abstract class InnerFragment extends Fragment implements OnGlobalLayoutListener, InnerIntf {

    protected View innerView = null;
    protected Bundle innerBundle = null;
    protected String tagText;
    protected String tabTitle;

    public InnerFragment() {
        super();
    }

    public InnerFragment(String tagText, String tabTitle) {
        super();
        this.tagText = tagText;
        this.tabTitle = tabTitle;
    }

    @Override
    public String getTagText() {
        return tagText;
    }

    public String getTabTitle() {
        return tabTitle;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        innerView = inflater.inflate(getFragmentLayoutResId(), container, false);
        initComponents();
        initEvents();
        innerView.getViewTreeObserver().addOnGlobalLayoutListener(this);
        return innerView;
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        innerBundle = getArguments();
        initLogic();
        if (getActivity().getActionBar() != null) {
            if (getCustomTitle() == null || getCustomTitle().equals("")) {
                if (UIInstance.dualPane) {
                    if (getBarTitleWithPath() != 0) {
                        getActivity().getActionBar().setTitle(getBarTitleWithPath());
                    }
                } else {
                    if (getBarTitle() != 0) {
                        getActivity().getActionBar().setTitle(getBarTitle());
                    }
                }
            } else {
                getActivity().getActionBar().setTitle(getCustomTitle());
            }
        }
    }

    public void setNewArguments(Bundle bn) {
        innerBundle = getArguments();
        onGetNewArguments(bn);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        if (getActivity() == null) {
            return;
        }
        if (getActivity().getClass().getName().equals(getMainActivityName()) && !UIInstance.dualPane) {
            return;
        }

        initMenu(menu);
    }

    @Override
    public void onGlobalLayout() {
        onLayoutReady();
    }

    protected void onLayoutReady() {

    }

}
