package com.rarnu.tools.root.fragmentactivity;

import android.app.AlertDialog;
import android.app.Fragment;
import android.view.KeyEvent;
import android.view.MenuItem;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.fragment.CompContainerFragment;

public class CompMainActivity extends BaseActivity {

    CompContainerFragment fCompContainer = null;

    @Override
    public Fragment replaceFragment() {
        if (fCompContainer == null) {
            fCompContainer = (CompContainerFragment) Fragments.getFragment(FragmentNameConst.FN_COMP);
        }
        return fCompContainer;
    }

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme ? android.R.style.Theme_Holo_Light : android.R.style.Theme_Holo;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (fCompContainer.getFragmentState().getBoolean("loading", false)) {
                confirmCancelLoading();
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                if (fCompContainer.getFragmentState().getBoolean("loading", false)) {
                    confirmCancelLoading();
                    return true;
                }
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    private void confirmCancelLoading() {
        new AlertDialog.Builder(this)
                .setTitle(R.string.hint)
                .setMessage(R.string.comp_loading_cannot_exit)
                .setPositiveButton(R.string.ok, null)
                .show();
    }
}
