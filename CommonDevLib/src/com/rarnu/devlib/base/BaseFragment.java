package com.rarnu.devlib.base;

import com.rarnu.devlib.base.inner.InnerFragment;


public abstract class BaseFragment extends InnerFragment {

    public BaseFragment() {
        super();
    }

    public BaseFragment(String tagText, String tabTitle) {
        super(tagText, tabTitle);
    }

}
