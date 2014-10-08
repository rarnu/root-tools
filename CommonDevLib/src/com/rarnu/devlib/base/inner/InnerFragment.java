package com.rarnu.devlib.base.inner;

import android.app.Fragment;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.*;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import com.rarnu.devlib.base.intf.InnerIntf;
import com.rarnu.devlib.common.UIInstance;
import com.rarnu.utils.DrawableUtils;
import com.rarnu.utils.ImageUtils;

public abstract class InnerFragment extends Fragment implements OnGlobalLayoutListener, InnerIntf {

    protected View innerView = null;
    protected Bundle innerBundle = null;
    protected String tabTitle;
    protected int tabIcon = -1;

    public InnerFragment() {
        super();
    }

    public InnerFragment(String tabTitle) {
        super();
        this.tabTitle = tabTitle;
    }

    public String getTabTitle() {
        return tabTitle;
    }

    public int getIcon() {
        return tabIcon;
    }

    public void setIcon(int res) {
        this.tabIcon = res;
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
