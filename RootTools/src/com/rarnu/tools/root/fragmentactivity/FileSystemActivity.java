package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import android.view.KeyEvent;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;

public class FileSystemActivity extends BaseActivity {

    BaseFragment fragmentFileSystem;

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        fragmentFileSystem = (BaseFragment) Fragments.getFragment(FragmentNameConst.FN_FILESYSTEM);
        return fragmentFileSystem;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (fragmentFileSystem != null) {
                fragmentFileSystem.setNewArguments(null);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }
}
