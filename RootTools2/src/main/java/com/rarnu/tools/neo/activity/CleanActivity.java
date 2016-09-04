package com.rarnu.tools.neo.activity;

import android.app.Fragment;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.MenuItem;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseActivity;
import com.rarnu.tools.neo.fragment.CleanFragment;

public class CleanActivity extends BaseActivity {

    private CleanFragment cf = new CleanFragment();

    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        return cf;
    }

    @Override
    public int customTheme() {
        return 0;
    }

    @Override
    public boolean getActionBarCanBack() {
        return true;
    }

    private boolean isCleaning() {
        Bundle bn = cf.getFragmentState();
        return bn.getBoolean("isCleaning", false);
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (isCleaning()) {
                Toast.makeText(this, R.string.toast_cleaning, Toast.LENGTH_SHORT).show();
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                if (isCleaning()) {
                    Toast.makeText(this, R.string.toast_cleaning, Toast.LENGTH_SHORT).show();
                    return true;
                }
                break;
        }
        return super.onOptionsItemSelected(item);
    }
}
