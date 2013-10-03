package com.yugioh.android;

import android.app.Fragment;
import android.os.Bundle;
import com.rarnu.devlib.base.BaseActivity;
import com.yugioh.android.fragments.AutoNameFragment;

public class AutoNameActivity extends BaseActivity {
    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        AutoNameFragment anf = new AutoNameFragment();
        Bundle bn = new Bundle();
        bn.putString("name", getIntent().getStringExtra("name"));
        anf.setArguments(bn);
        return anf;
    }
}
