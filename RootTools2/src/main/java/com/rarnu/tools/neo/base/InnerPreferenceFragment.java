package com.rarnu.tools.neo.base;

import android.os.Bundle;
import android.preference.PreferenceFragment;
import android.view.Menu;
import android.view.MenuInflater;

public abstract class InnerPreferenceFragment extends PreferenceFragment implements IIntf {

    public String tabTitle = null;
    protected Bundle innerBundle = null;

    public InnerPreferenceFragment() {
        super();
    }
    public InnerPreferenceFragment(String tabTitle) {
        super();
        this.tabTitle = tabTitle;
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        innerBundle = getArguments();
        initComponents();
        initEvents();
        initLogic();
    }

    public void setNewArguments(Bundle bn) {
        innerBundle = bn;
        onGetNewArguments(bn);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
        addPreferencesFromResource(getFragmentLayoutResId());
        if (getActivity().getActionBar() != null) {
            if (getCustomTitle() == null || getCustomTitle().equals("")) {
                getActivity().getActionBar().setTitle(getBarTitle());
            } else {
                getActivity().getActionBar().setTitle(getCustomTitle());
            }
        }
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        if (getActivity() == null) {
            return;
        }
        initMenu(menu);
    }

}
