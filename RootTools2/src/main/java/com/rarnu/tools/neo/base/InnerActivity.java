package com.rarnu.tools.neo.base;

import android.app.ActionBar;
import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.ViewTreeObserver;
import android.view.Window;
import android.widget.RelativeLayout;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.utils.DrawableUtils;
import com.rarnu.tools.neo.utils.UIUtils;

public abstract class InnerActivity extends Activity implements ViewTreeObserver.OnGlobalLayoutListener {

    protected ActionBar bar = null;
    protected RelativeLayout layoutReplacement = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        if (customTheme() != 0) {
            setTheme(customTheme());
        }
        requestWindowFeature(Window.FEATURE_ACTION_BAR);
        super.onCreate(savedInstanceState);
        if (getCloseCondition()) {
            finish();
            return;
        }
        setContentView(getBaseLayout());
        layoutReplacement = (RelativeLayout)findViewById(R.id.layoutReplacement);
        layoutReplacement.getViewTreeObserver().addOnGlobalLayoutListener(this);
        layoutReplacement.setBackground(UIUtils.isFollowSystemBackground() ? DrawableUtils.getDetailsElementBackground(this): null);

        bar = getActionBar();
        if (bar != null) {
            bar.setIcon(getIcon());
            if (getActionBarCanBack()) {
                bar.setDisplayOptions(0, ActionBar.DISPLAY_HOME_AS_UP);
                bar.setDisplayHomeAsUpEnabled(true);
            }
        }
        replace();
    }

    public abstract int getIcon();
    public abstract boolean getCloseCondition();
    public abstract int getBaseLayout();
    public abstract int getReplaceId();
    public abstract Fragment replaceFragment();
    public abstract int customTheme();
    public abstract boolean getActionBarCanBack();

    public void replace() {
        Fragment bf = replaceFragment();
        getFragmentManager().beginTransaction().replace(getReplaceId(), bf).commit();
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                finish();
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public void onGlobalLayout() {
        onLayoutReady();
    }

    /**
     * override the method if you want to re-layout after system layouted
     */
    public void onLayoutReady() {

    }

}
