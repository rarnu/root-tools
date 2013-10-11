package com.rarnu.ucloud.android.fragment;

import android.os.Bundle;
import android.view.Menu;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.ucloud.android.R;
import com.rarnu.utils.ResourceUtils;

public class ServiceFragment extends BaseFragment {

    public ServiceFragment() {
        super();
        tagText = ResourceUtils.getString(R.string.tag_service_fragment);
        tabTitle = ResourceUtils.getString(R.string.title_service_fragment);
    }

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
    }

    @Override
    public void initEvents() {
    }

    @Override
    public void initLogic() {
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_service;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }
}
