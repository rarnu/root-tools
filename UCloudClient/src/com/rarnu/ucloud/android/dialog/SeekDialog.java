package com.rarnu.ucloud.android.dialog;

import android.app.Fragment;
import android.os.Bundle;
import com.rarnu.devlib.base.BaseDialog;
import com.rarnu.ucloud.android.fragment.SeekFragment;

public class SeekDialog extends BaseDialog {
    @Override
    public boolean getCondition() {
        return false;
    }

    @Override
    public Fragment replaceFragment() {

        Bundle bn = new Bundle();
        bn.putString("title", getIntent().getStringExtra("title"));
        bn.putInt("progress", getIntent().getIntExtra("progress", 0));
        SeekFragment sf = new SeekFragment();
        sf.setArguments(bn);
        return sf;
    }
}
