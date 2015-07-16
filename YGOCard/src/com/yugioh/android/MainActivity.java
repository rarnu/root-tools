package com.yugioh.android;

import android.app.Fragment;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.view.KeyEvent;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.base.inner.InnerFragment;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.yugioh.android.common.Actions;
import com.yugioh.android.database.YugiohDatabase;
import com.yugioh.android.fragments.*;
import com.yugioh.android.intf.IMainIntf;
import com.yugioh.android.utils.UpdateUtils;

public class MainActivity extends BaseSlidingActivity implements IMainIntf {

    int currentPage = 0;
    private IntentFilter filterClose = new IntentFilter(Actions.ACTION_CLOSE_MAIN);
    private CloseReceiver receiverClose = new CloseReceiver();
    private DatabaseMessageReceiver receiverDatabase = new DatabaseMessageReceiver();
    private IntentFilter filterDatabase = new IntentFilter();

    @Override
    public void onCreate(Bundle savedInstanceState) {

        filterDatabase.addAction(Actions.ACTION_EXTRACT_DATABASE);
        filterDatabase.addAction(Actions.ACTION_EXTRACT_DATABASE_COMPLETE);
        registerReceiver(receiverDatabase, filterDatabase);
        super.onCreate(savedInstanceState);

        if (!YugiohDatabase.isDatabaseFileExists()) {
            Toast.makeText(this, R.string.main_update_database, Toast.LENGTH_LONG).show();
        } else {
            UpdateUtils.updateLocalDatabase(this);
        }
        registerReceiver(receiverClose, filterClose);
    }

    @Override
    protected void onDestroy() {
        try {
            unregisterReceiver(receiverClose);
        } catch (Exception e) {

        }
        try {
            unregisterReceiver(receiverDatabase);
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
        return new SearchFragment();
    }

    @Override
    public int customTheme() {
        return 0;
    }

    @Override
    public void switchPage(int page, boolean needToggle) {
        if (currentPage != page) {
            currentPage = page;
            Fragment f = getCurrentFragment(currentPage);
            if (!f.isAdded()) {
                getFragmentManager().beginTransaction().replace(R.id.fReplacement, f).commit();
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
                f = new SearchFragment();
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

    private void terminateSelf() {
        finish();
        android.os.Process.killProcess(android.os.Process.myPid());
        System.exit(0);
    }

    public class CloseReceiver extends BroadcastReceiver {

        @Override
        public void onReceive(Context context, Intent intent) {
            terminateSelf();
        }
    }

    public class DatabaseMessageReceiver extends BroadcastReceiver {

        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            if (action.equals(Actions.ACTION_EXTRACT_DATABASE_COMPLETE)) {
                Toast.makeText(MainActivity.this, R.string.main_updated_database, Toast.LENGTH_SHORT).show();
            }
        }
    }
}
