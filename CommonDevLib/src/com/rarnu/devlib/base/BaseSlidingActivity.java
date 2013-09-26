package com.rarnu.devlib.base;

import android.app.Fragment;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.widget.FrameLayout;
import com.rarnu.devlib.R;
import com.rarnu.devlib.base.inner.InnerActivity;
import com.rarnu.devlib.base.intf.InnerIntf;
import com.rarnu.devlib.common.IFragments;
import com.rarnu.devlib.common.ISliding;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.devlib.component.tools.SlidingHelper;
import com.rarnu.utils.DrawableUtils;
import com.rarnu.utils.UIUtils;

public abstract class BaseSlidingActivity extends InnerActivity implements ISliding, IFragments {

    private SlidingHelper mHelper;

    @Override
    public boolean getCondition() {
        return false;
    }

    @Override
    public int getBaseLayout() {
        return R.layout.layout_replacement;
    }

    @Override
    public int getReplaceId() {
        return R.id.fReplacement;
    }

    public abstract Fragment replaceMenuFragment();

    public abstract Fragment replaceSecondMenuFragment();

    public abstract int getBehindOffset();

    public abstract int getAboveTouchMode();

    public abstract int getBehindTouchMode();

    public abstract int getSlideMode();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        mHelper = new SlidingHelper(this);
        mHelper.onCreate(savedInstanceState);

        loadFragments();

        super.onCreate(savedInstanceState);
        setBehindContentView(R.layout.layout_menu_replacement);

        ((FrameLayout) findViewById(R.id.menu)).setBackgroundDrawable(UIUtils.isFollowSystemBackground() ? DrawableUtils.getSystemAttrDrawable(this, DrawableUtils.DETAILS_ELEMENT_BACKGROUND) : null);

        SlidingMenu sm = getSlidingMenu();
        sm.setShadowWidth(15);
        sm.setShadowDrawable(R.drawable.shadow);
        sm.setBehindOffset(getBehindOffset());
        sm.setFadeDegree(0.35f);
        sm.setTouchModeAbove(getAboveTouchMode());
        sm.setTouchModeBehind(getBehindTouchMode());
        sm.setMode(getSlideMode());
        if (sm.getMode() == SlidingMenu.LEFT || sm.getMode() == SlidingMenu.LEFT_RIGHT) {
            replaceMenu();
        }
        if (sm.getMode() == SlidingMenu.LEFT_RIGHT || sm.getMode() == SlidingMenu.RIGHT) {
            sm.setSecondaryMenu(R.layout.layout_second_menu_replacement);
            ((FrameLayout) findViewById(R.id.second_menu)).setBackgroundDrawable(UIUtils.isFollowSystemBackground() ? DrawableUtils.getSystemAttrDrawable(this, DrawableUtils.DETAILS_ELEMENT_BACKGROUND) : null);
            sm.setSecondaryShadowDrawable(R.drawable.shadow);
            replaceSecondMenu();
        }
    }

    @Override
    protected void onDestroy() {
        releaseFragments();
        super.onDestroy();
    }

    public void replaceMenu() {
        Fragment bf = replaceMenuFragment();
        getFragmentManager().beginTransaction().replace(R.id.menu, bf, ((InnerIntf) bf).getTagText()).commit();
    }

    public void replaceSecondMenu() {
        Fragment bf = replaceSecondMenuFragment();
        getFragmentManager().beginTransaction().replace(R.id.second_menu, bf, ((InnerIntf) bf).getTagText()).commit();
    }

    @Override
    public void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);
        mHelper.onPostCreate(savedInstanceState);
    }

    @Override
    public View findViewById(int id) {
        View v = super.findViewById(id);
        if (v != null) {
            return v;
        }
        return mHelper.findViewById(id);
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        mHelper.onSaveInstanceState(outState);
    }

    @Override
    public void setContentView(int id) {
        setContentView(getLayoutInflater().inflate(id, null));
    }

    @Override
    public void setContentView(View v) {
        setContentView(v, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
    }

    @Override
    public void setContentView(View v, LayoutParams params) {
        super.setContentView(v, params);
        mHelper.registerAboveContentView(v, params);
    }

    public void setBehindContentView(int id) {
        setBehindContentView(getLayoutInflater().inflate(id, null));
    }

    public void setBehindContentView(View v) {
        setBehindContentView(v, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
    }

    public void setBehindContentView(View v, LayoutParams params) {
        mHelper.setBehindContentView(v, params);
    }

    public SlidingMenu getSlidingMenu() {
        return mHelper.getSlidingMenu();
    }

    public void toggle() {
        mHelper.toggle();
    }

    public void showContent() {
        mHelper.showContent();
    }

    public void showMenu() {
        mHelper.showMenu();
    }

    public void showSecondaryMenu() {
        mHelper.showSecondaryMenu();
    }

    public void setSlidingActionBarEnabled(boolean b) {
        mHelper.setSlidingActionBarEnabled(b);
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
        boolean b = mHelper.onKeyUp(keyCode, event);
        if (b) {
            return b;
        }
        return super.onKeyUp(keyCode, event);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                toggle();
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

}
