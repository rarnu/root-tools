package com.rarnu.devlib.base.inner;

import android.os.Bundle;
import android.preference.PreferenceFragment;
import android.view.Menu;
import android.view.MenuInflater;
import com.rarnu.devlib.base.intf.InnerIntf;
import com.rarnu.devlib.common.UIInstance;

public abstract class InnerPreferenceFragment extends PreferenceFragment implements InnerIntf {

    protected String tagText;
    protected String tabTitle;
    protected Bundle innerBundle = null;

    public InnerPreferenceFragment() {
        super();
    }

    public InnerPreferenceFragment(String tagText, String tabTitle) {
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
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        innerBundle = getArguments();
        initComponents();
        initEvents();
        initLogic();
    }

    public void setNewArguments(Bundle bn) {
        innerBundle = getArguments();
        onGetNewArguments(bn);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
        addPreferencesFromResource(getFragmentLayoutResId());

        if (getActivity().getActionBar() != null) {
            if (getCustomTitle() == null || getCustomTitle().equals("")) {
                if (UIInstance.dualPane) {
                    getActivity().getActionBar().setTitle(getBarTitleWithPath());
                } else {
                    getActivity().getActionBar().setTitle(getBarTitle());
                }
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
        if (getActivity().getClass().getName().equals(getMainActivityName()) && !UIInstance.dualPane) {
            return;
        }

        initMenu(menu);
    }

}
