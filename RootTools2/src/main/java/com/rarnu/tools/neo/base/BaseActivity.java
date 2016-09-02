package com.rarnu.tools.neo.base;

import android.content.res.Configuration;
import com.rarnu.tools.neo.R;

public abstract class BaseActivity extends InnerActivity {

    @Override
    public boolean getCloseCondition() {
        return getResources().getConfiguration().orientation == Configuration.ORIENTATION_LANDSCAPE;
    }

    @Override
    public int getBaseLayout() {
        return R.layout.layout_replacement;
    }

    @Override
    public int getReplaceId() {
        return R.id.fReplacement;
    }
}
