package com.rarnu.ucloud.android;

import android.app.ActionBar;
import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.view.MenuItem;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.ucloud.android.fragment.MainFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

public class MainActivity extends BaseActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        ResourceUtils.init(this);
        UIUtils.initDisplayMetrics(this, getWindowManager(), true);
        super.onCreate(savedInstanceState);
        bar.setDisplayHomeAsUpEnabled(false);

    }

    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        return new MainFragment();
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            return true;
        }
        return super.onOptionsItemSelected(item);
    }
}
