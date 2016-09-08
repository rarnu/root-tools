package com.rarnu.tools.neo.base;

import android.app.Fragment;
import android.content.Context;
import android.os.Build;
import android.os.Bundle;
import android.view.*;

public abstract class InnerFragment extends Fragment implements ViewTreeObserver.OnGlobalLayoutListener, IIntf {

    protected View innerView = null;
    protected Bundle innerBundle = null;
    public String tabTitle = null;
    public int tabIcon = -1;

    public InnerFragment() {
        super();
    }

    public InnerFragment(String tabTitle) {
        super();
        this.tabTitle = tabTitle;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        if (getFragmentLayoutResId() != 0) {
            innerView = inflater.inflate(getFragmentLayoutResId(), container, false);
        } else {
            innerView = getFramgmentLayoutView();
        }
        initComponents();
        initEvents();
        innerView.getViewTreeObserver().addOnGlobalLayoutListener(this);
        return innerView;
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        if (innerBundle == null) {
            innerBundle = getArguments();
        }
        initLogic();
        if (getActivity().getActionBar() != null) {
            if (getCustomTitle() == null || getCustomTitle().equals("")) {
                if (getBarTitle() != 0) {
                    getActivity().getActionBar().setTitle(getBarTitle());
                }
            } else {
                getActivity().getActionBar().setTitle(getCustomTitle());
            }
        }
    }

    public void setNewArguments(Bundle bn) {
        innerBundle = bn;
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
        initMenu(menu);
    }



    /**
     * do NOT override this method, use @onLayoutReady instead
     */
    @Override
    public void onGlobalLayout() {
        onLayoutReady();
    }

    /**
     * override the method if you want to re-layout after system layouted
     */
    protected void onLayoutReady() {

    }

    /**
     * override the method if you do not need a layout from resource and @getFragmentLayoutResId returns 0
     */
    protected View getFramgmentLayoutView() {
        return null;
    }

    public Context getContext() {
        if (Build.VERSION.SDK_INT >= 23) {
            return super.getContext();
        } else {
            return super.getActivity();
        }
    }
}
