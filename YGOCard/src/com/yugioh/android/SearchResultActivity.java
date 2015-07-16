package com.yugioh.android;

import android.app.Fragment;
import com.rarnu.devlib.base.BaseActivity;
import com.yugioh.android.fragments.SearchResultFragment;

/**
 * Created by rarnu on 7/16/15.
 */
public class SearchResultActivity extends BaseActivity {
    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        SearchResultFragment srf = new SearchResultFragment();
        return srf;
    }

    @Override
    public int customTheme() {
        return 0;
    }
}
