package com.yugioh.android;

import android.app.AlertDialog;
import android.app.Fragment;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.MenuItem;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.utils.DownloadUtils;
import com.yugioh.android.fragments.UpdateFragment;
import com.yugioh.android.intf.IUpdateIntf;

public class UpdateActivity extends BaseActivity implements IUpdateIntf {

    UpdateFragment uf = null;
    private boolean inProgress = false;
    private String localDir;
    private String localFile;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        uf = new UpdateFragment();
        super.onCreate(savedInstanceState);
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (inProgress) {
                confirmCloseUpdate();
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                if (inProgress) {
                    confirmCloseUpdate();
                    return true;
                } else {
                    finish();
                }
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    private void confirmCloseUpdate() {
        new AlertDialog.Builder(this)
                .setTitle(R.string.hint)
                .setMessage(R.string.update_downloading)
                .setPositiveButton(R.string.ok, new OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        DownloadUtils.stopDownloadTask(localDir, localFile);
                        uf.doDestroyHandler();
                        finish();
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    @Override
    public boolean isInProgress() {
        return inProgress;
    }

    @Override
    public void setInProgress(boolean inProgress) {
        this.inProgress = inProgress;
    }

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        return uf;
    }

    @Override
    public int customTheme() {
        return 0;
    }

    @Override
    public void setUpdateFile(String localDir, String localFile) {
        this.localDir = localDir;
        this.localFile = localFile;
    }

}
