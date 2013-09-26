package com.rarnu.devlib.base;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import com.rarnu.devlib.base.intf.InnerIntf;

public abstract class BaseDialogFragment extends Fragment implements InnerIntf {

    protected View innerView = null;
    protected String tagText;

    public BaseDialogFragment() {
        super();
    }

    public BaseDialogFragment(String tagText) {
        super();
        this.tagText = tagText;
    }

    @Override
    public String getTagText() {
        return tagText;
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
