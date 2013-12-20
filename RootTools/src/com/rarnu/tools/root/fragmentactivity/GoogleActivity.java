package com.rarnu.tools.root.fragmentactivity;

import android.app.AlertDialog;
import android.app.Fragment;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.MenuItem;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.fragment.GoogleFragment;

public class GoogleActivity extends BaseActivity {

    GoogleFragment gf = null;

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        if (gf == null) {
            gf = (GoogleFragment) Fragments.getFragment(FragmentNameConst.FN_GOOGLE);
        }
        return gf;
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme ? android.R.style.Theme_Holo_Light : android.R.style.Theme_Holo;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (gf.getFragmentState().getBoolean("downloading", false)) {
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
                if (gf.getFragmentState().getBoolean("downloading", false)) {
                    confirmCancelDownload();
                    return true;
                } else {
                    finish();
                }
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    private void confirmCancelDownload() {
        new AlertDialog.Builder(this)
                .setTitle(R.string.hint)
                .setMessage(R.string.google_downloading_zip)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        Bundle bn = new Bundle();
                        bn.putBoolean("cancel", true);
                        gf.setNewArguments(bn);
                        finish();

                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }
}
