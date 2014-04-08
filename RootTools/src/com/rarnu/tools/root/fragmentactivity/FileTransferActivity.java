package com.rarnu.tools.root.fragmentactivity;

import android.app.AlertDialog;
import android.app.Fragment;
import android.os.Bundle;
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
            Bundle bn = fTransfer.getFragmentState();
            if (bn != null && bn.getBoolean("inOperating")) {
                confirmExit(bn.getInt("mode", -1) == 0);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme ? android.R.style.Theme_Holo_Light : android.R.style.Theme_Holo;
    }

    private void confirmExit(boolean isSend) {
        new AlertDialog.Builder(this)
                .setTitle(R.string.hint)
                .setMessage(isSend ? R.string.ft_sending_cannot_exit : R.string.ft_receiving_cannot_exit)
                .setPositiveButton(R.string.ok, null)
                .show();
    }
}
