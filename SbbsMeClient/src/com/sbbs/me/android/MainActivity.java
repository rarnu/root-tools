package com.sbbs.me.android;

import android.app.Fragment;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.KeyEvent;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.base.inner.InnerFragment;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.devlib.component.intf.OnCloseListener;
import com.rarnu.devlib.component.intf.OnOpenListener;
import com.rarnu.utils.DeviceUtilsLite;
import com.rarnu.utils.NotificationUtils;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeUpdate;
import com.sbbs.me.android.consts.Actions;
import com.sbbs.me.android.fragment.*;
import com.sbbs.me.android.service.MessageService;

public class MainActivity extends BaseSlidingActivity implements IMainIntf,
        OnOpenListener, OnCloseListener {

    final Handler hUpdate = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                SbbsMeUpdate update = (SbbsMeUpdate) msg.obj;
                NotificationUtils.cancelNotication(MainActivity.this,
                        Actions.ACTION_NOTIFY_UPDATE);
                NotificationUtils.showNotification(MainActivity.this,
                        Actions.ACTION_NOTIFY_UPDATE, R.drawable.logo48,
                        R.string.notify_update_title,
                        R.string.notify_update_desc,
                        Actions.ACTION_NOTIFY_UPDATE_CLICK, update, true);
            }
            super.handleMessage(msg);
        }

        ;
    };
    int currentPage = 0;
    boolean donotCloseTag = false;
    boolean returnToHome = false;
    MainFragment mf = null;
    PostNewFragment pnf = null;
    HotTagsFragment htf = null;
    OnGithubTabFragment ogtf = null;
    ArchievementFragment af = null;
    RecentFragment rf = null;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        UIUtils.initDisplayMetrics(this, getWindowManager(), false);
        ResourceUtils.init(this);
        Global.autoRefreshTag = true;
        Global.canExit = false;
        super.onCreate(savedInstanceState);
        startService(new Intent(this, MessageService.class));
        getSlidingMenu().setOnOpenListener(this);
        getSlidingMenu().setOnCloseListener(this);
        doCheckUpdateT(this);
    }

    private void doCheckUpdateT(final Context context) {

        new Thread(new Runnable() {

            @Override
            public void run() {
                SbbsMeUpdate update = SbbsMeAPI.checkUpdate(DeviceUtilsLite
                        .getAppVersionCode(MainActivity.this));
                if (update != null && update.needUpdate) {
                    Message msg = new Message();
                    msg.what = 1;
                    msg.obj = update;
                    hUpdate.sendMessage(msg);
                }
            }
        }).start();
    }

    @Override
    protected void onDestroy() {
        SbbsMeAPI.logout();
        Global.releaseAll();
        super.onDestroy();
    }

    @Override
    public void loadFragments() {
        mf = new MainFragment();
        pnf = new PostNewFragment();
        htf = new HotTagsFragment();
        ogtf = new OnGithubTabFragment();
        af = new ArchievementFragment();
        rf = new RecentFragment();
    }

    @Override
    public void releaseFragments() {

    }

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
        return UIUtils.dipToPx(150);
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
    public int getIcon() {
        return R.drawable.inner_logo;
    }

    @Override
    public Fragment replaceFragment() {
        return new MainFragment();
    }

    @Override
    public int customTheme() {
        return 0;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (currentPage != 0 && keyCode == KeyEvent.KEYCODE_BACK
                && (!getSlidingMenu().isMenuShowing())) {
            donotCloseTag = true;
            returnToHome = true;
            showMenu();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
        if (donotCloseTag && keyCode == KeyEvent.KEYCODE_BACK) {
            donotCloseTag = false;
            return true;
        }
        if (returnToHome) {
            returnToHome = false;
            switchPage(0, true);
            return true;
        }
        if (Global.canExit) {
            finish();
        } else {
            Global.canExit = true;
            Toast.makeText(this, R.string.exit_hint, Toast.LENGTH_SHORT).show();
            return true;
        }
        return super.onKeyUp(keyCode, event);
    }

    @Override
    public void switchPage(int page, boolean needToggle) {
        Global.canExit = false;
        if (currentPage != page) {
            currentPage = page;
            Fragment f = getCurrentFragment(currentPage);
            if (!f.isAdded()) {
                getFragmentManager()
                        .beginTransaction()
                        .replace(R.id.fReplacement, f).commit();
            }
            getActionBar().setTitle(
                    getString(((InnerFragment) f).getBarTitle()));
        }
        if (needToggle) {
            toggle();
        }
    }

    private Fragment getCurrentFragment(int page) {
        Fragment f = null;
        switch (page) {
            case 0:
                // MAIN
                f = mf;
                break;
            case 1:
                // post new
                f = pnf;
                break;
            case 2:
                // recent
                f = rf;
                break;
            case 3:
                // hot tags
                f = htf;
                break;
            case 4:
                // on github
                f = ogtf;
                break;
            case 5:
                // archievement
                f = af;
                break;
        }
        return f;
    }

    @Override
    public void onClose() {
        Global.canExit = false;

    }

    @Override
    public void onOpen() {
        Global.canExit = false;

    }

}
