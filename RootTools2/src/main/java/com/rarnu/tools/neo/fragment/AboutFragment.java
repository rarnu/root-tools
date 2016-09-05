package com.rarnu.tools.neo.fragment;

import android.os.Bundle;
import android.view.Menu;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseFragment;

public class AboutFragment extends BaseFragment {

    // TODO: about

    @Override
    public int getBarTitle() {
        return R.string.about_name;
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
        return R.layout.fragment_about;
    }

    @Override
    public String getMainActivityName() {
        return null;
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
