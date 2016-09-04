package com.rarnu.tools.neo.base;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public abstract class BaseDialogFragment extends Fragment implements IIntf {

    protected View innerView = null;

    public BaseDialogFragment() {
        super();
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        innerView = inflater.inflate(getFragmentLayoutResId(), container, false);
        initComponents();
        initEvents();
        initLogic();
        return innerView;
    }

}
