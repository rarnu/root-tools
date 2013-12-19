package com.rarnu.startup;

import android.app.Fragment;
import android.os.Bundle;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.startup.fragment.ArticleFragment;

public class ArticleActivity extends BaseActivity {
    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        ArticleFragment af = new ArticleFragment();
        Bundle bn = new Bundle();
        bn.putLong("id", getIntent().getLongExtra("id", 0L));
        af.setArguments(bn);
        return af;
    }

    @Override
    public int customTheme() {
        return 0;
    }
}
