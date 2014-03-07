package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;

public class BuildPropFragment extends BaseFragment {

    @Override
    public int getBarTitle() {
        return R.string.func_build_prop;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_build_prop_with_path;
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
        return R.layout.layout_build_prop;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
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
