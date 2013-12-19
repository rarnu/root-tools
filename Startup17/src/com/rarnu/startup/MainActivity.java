package com.rarnu.startup;

import android.app.Fragment;
import android.os.Bundle;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.startup.fragment.LeftMenuFragment;
import com.rarnu.startup.fragment.MainFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

public class MainActivity extends BaseSlidingActivity {

    MainFragment mf = null;

    @Override
    public Fragment replaceMenuFragment() {
        return new LeftMenuFragment();
    }

    @Override
    public Fragment replaceSecondMenuFragment() {
        return null;
    }

    @Override
    public int getBehindOffset() {
        return UIUtils.dipToPx(200);
    }

    @Override
    public int getAboveTouchMode() {
        return SlidingMenu.TOUCHMODE_MARGIN;
    }

    @Override
    public int getBehindTouchMode() {
        return SlidingMenu.TOUCHMODE_MARGIN;
    }

    @Override
    public int getSlideMode() {
        return SlidingMenu.LEFT;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        UIUtils.initDisplayMetrics(this, getWindowManager(), false);
        ResourceUtils.init(this);
        super.onCreate(savedInstanceState);
    }

    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        if (mf == null) {
            mf = new MainFragment();
        }
        return mf;
    }

    @Override
    public int customTheme() {
        return 0;
    }

    @Override
    public void loadFragments() {

    }

    @Override
    public void releaseFragments() {

    }
}
