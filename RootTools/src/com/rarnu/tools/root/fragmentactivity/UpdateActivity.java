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
import com.rarnu.tools.root.fragment.UpdateFragment;

public class UpdateActivity extends BaseActivity {

    UpdateFragment uf = null;

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        if (uf == null) {
            uf = (UpdateFragment) Fragments.getFragment(FragmentNameConst.FN_UPDATE);
        }
        return uf;
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme ? android.R.style.Theme_Holo_Light : android.R.style.Theme_Holo;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (uf.getFragmentState().getBoolean("downloading", false)) {
                confirmCancelDownload();
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                if (uf.getFragmentState().getBoolean("downloading", false)) {
                    confirmCancelDownload();
                    return true;
                }
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    private void confirmCancelDownload() {
        new AlertDialog.Builder(this)
                .setTitle(R.string.hint)
                .setMessage(R.string.updating_cannot_exit)
                .setPositiveButton(R.string.ok, null)
                .show();
    }
}
