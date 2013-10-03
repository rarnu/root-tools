package com.yugioh.android;

import android.app.AlertDialog;
import android.app.Fragment;
import android.content.*;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.KeyEvent;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.base.inner.InnerFragment;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.MiscUtils;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.database.YugiohDatabase;
import com.yugioh.android.define.PathDefine;
import com.yugioh.android.fragments.*;
import com.yugioh.android.intf.IMainIntf;
import com.yugioh.android.utils.UpdateUtils;

public class MainActivity extends BaseSlidingActivity implements IMainIntf {

    public static final String ACTION_CLOSE_MAIN = "com.yugioh.android.close.main";
    public CloseReceiver receiverClose = new CloseReceiver();
    public IntentFilter filterClose = new IntentFilter(ACTION_CLOSE_MAIN);
    int currentPage = 0;
    private Handler hUpdate = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                UpdateInfo updateInfo = (UpdateInfo) msg.obj;
                if (updateInfo != null) {
                    int hasUpdate = updateInfo.getUpdateApk() + updateInfo.getUpdateData();
                    if (hasUpdate != 0) {
                        Toast.makeText(MainActivity.this, R.string.update_hint, Toast.LENGTH_LONG).show();
                    }
                }

            }
            super.handleMessage(msg);
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        UIUtils.initDisplayMetrics(this, getWindowManager(), false);
        ResourceUtils.init(this);
        PathDefine.init();
        super.onCreate(savedInstanceState);
        registerReceiver(receiverClose, filterClose);
        if (!MiscUtils.isSDCardExists()) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.hint)
                    .setMessage(R.string.no_sdcard)
                    .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            finish();
                        }
                    })
                    .show();
            return;
        }
        if (!YugiohDatabase.isDatabaseFileExists()) {
            UpdateInfo updateInfo = new UpdateInfo();
            updateInfo.setUpdateApk(0);
            updateInfo.setUpdateData(1);
            Intent inUpdate = new Intent(this, UpdateActivity.class);
            inUpdate.putExtra("update", updateInfo);
            startActivity(inUpdate);
            finish();
        } else {
            checkUpdate();
        }
    }

    private void checkUpdate() {
        UpdateUtils.checkUpdateT(this, hUpdate);
    }

    @Override
    protected void onDestroy() {
        try {
            unregisterReceiver(receiverClose);
        } catch (Exception e) {

        }
        super.onDestroy();
    }

    @Override
    public void loadFragments() {

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
        return new RightMenuFragment();
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
        return SlidingMenu.LEFT_RIGHT;
    }

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        return new MainFragment();
    }

    @Override
    public void switchPage(int page, boolean needToggle) {
        if (currentPage != page) {
            currentPage = page;
            Fragment f = getCurrentFragment(currentPage);
            if (!f.isAdded()) {
                getFragmentManager().beginTransaction().replace(R.id.fReplacement, f, ((InnerFragment) f).getTagText()).commit();
            }
            getActionBar().setTitle(getString(((InnerFragment) f).getBarTitle()));
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
                f = new MainFragment();
                break;
            case 1:
                // LIMIT
                f = new LimitFragment();
                break;
            case 2:
                // NEW CARD
                f = new NewCardFragment();
                break;
            case 3:
                // PACKAGE
                f = new PackageListFragment();
                break;
            case 4:
                // my fav
                f = new FavFragment();
                break;
            case 5:
                // DUEL TOOL
                f = new DuelToolFragment();
                break;
        }
        return f;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_MENU) {
            toggle();
            return true;
        } else if (currentPage != 0 && keyCode == KeyEvent.KEYCODE_BACK) {
            switchPage(0, false);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    public class CloseReceiver extends BroadcastReceiver {

        @Override
        public void onReceive(Context context, Intent intent) {
            MainActivity.this.finish();
        }
    }

}
