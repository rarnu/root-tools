package com.rarnu.tools.root.fragmentactivity;

import android.app.AlertDialog;
import android.app.Fragment;
import android.view.KeyEvent;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;

public class FileTransferActivity extends BaseActivity {

    BaseFragment fTransfer;

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        fTransfer = (BaseFragment) Fragments.getFragment(FragmentNameConst.FN_FILE_TRANSFER);
        return fTransfer;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (fTransfer != null) {
            if (fTransfer.getFragmentState().getBoolean("inOperating")) {
                confirmExit();
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme ? android.R.style.Theme_Holo_Light : android.R.style.Theme_Holo;
    }

    private void confirmExit() {
        new AlertDialog.Builder(this)
                .setTitle(R.string.hint)
                .setMessage(R.string.ft_operating_cannot_exit)
                .setPositiveButton(R.string.ok, null)
                .show();
    }
}
