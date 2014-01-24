package com.rarnu.tools.root.fragment;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;
import com.rarnu.devlib.base.BaseTabFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;

import java.util.List;

public class BatchAppsFragment extends BaseTabFragment {

    BatchAppInstallFragment fBatchInstall;
    BatchAppUninstallFragment fBatchUninstall;

    @Override
    public void initFragmentList(List<Fragment> listFragment) {
        listFragment.add(fBatchInstall);
        listFragment.add(fBatchUninstall);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        fBatchInstall = new BatchAppInstallFragment();
        fBatchUninstall = new BatchAppUninstallFragment();
    }

    @Override
    public int getBarTitle() {
        return R.string.func_batch_app;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_batch_app_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
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
