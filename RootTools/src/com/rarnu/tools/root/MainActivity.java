package com.rarnu.tools.root;

import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.AndroidCharacter;
import android.view.Menu;
import android.view.MenuItem;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseMainActivity;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.fragmentactivity.ShareActivity;
import com.rarnu.tools.root.service.FloatWidgetService;
import com.rarnu.tools.root.utils.CustomPackageUtils;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.UpdateUtils;
import com.rarnu.utils.FloatUtils;
import com.rarnu.utils.NetworkUtils;

public class MainActivity extends BaseMainActivity {

    final Handler hUpdate = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                if (GlobalInstance.updateInfo != null && GlobalInstance.updateInfo.result != 0) {
                    UpdateUtils.showUpdateInfo(MainActivity.this);
                }
            }
            super.handleMessage(msg);
        }
    };
    MenuItem actionItem;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }

    private void loadExcludeListT() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                MemorySpecialList.loadExcludeList(MainActivity.this);
            }
        }).start();
    }

    public void loadCustomPackageListT() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                CustomPackageUtils.loadCustomPackages();
            }
        }).start();
    }

    private void loadNetworkStatus() {
        NetworkUtils.doGetNetworkInfoT(this);
    }

    private void getUpdateInfo() {
        new Thread(new Runnable() {

            @Override
            public void run() {
                int verCode = DeviceUtils.getAppVersionCode(MainActivity.this);
                GlobalInstance.updateInfo = MobileApi.checkUpdate(verCode);
                hUpdate.sendEmptyMessage(1);
            }
        }).start();
    }

    @Override
    public void loadFragments() {

    }

    @Override
    public void releaseFragments() {
        Fragments.releaseFragments();

    }

    @Override
    public void initOnce() {
        RTConfig.initConfig(this);
        RootUtils.mountRW();
        loadExcludeListT();
        loadCustomPackageListT();

        if (GlobalInstance.isFirstStart) {

            GlobalInstance.isFirstStart = false;
            RTConfig.setFirstStart(this, GlobalInstance.isFirstStart);
        }

        if (GlobalInstance.showFloatWindow) {
            if (!FloatWidgetService.isAlive) {
                FloatUtils.showFloatWindow(this, FloatWidgetService.class);
            }
        }

        loadNetworkStatus();
        getUpdateInfo();

    }

    @Override
    public String getBarTitle() {
        return getString(R.string.app_name);
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme? android.R.style.Theme_Holo_Light: android.R.style.Theme_Holo;
    }

    @Override
    public Fragment getFragment(int currentFragment) {
        switch (currentFragment) {
            case 1:
                return Fragments.getFragment(FragmentNameConst.FN_SYSAPP);
            case 2:
                return Fragments.getFragment(FragmentNameConst.FN_ENABLEAPP);
            case 3:
                return Fragments.getFragment(FragmentNameConst.FN_COMP);
            case 4:
                return Fragments.getFragment(FragmentNameConst.FN_SYSTEM_COMPONENT);
            case 5:
                return Fragments.getFragment(FragmentNameConst.FN_HTCROM);
            case 6:
                return Fragments.getFragment(FragmentNameConst.FN_BACKUP);
            case 7:
                return Fragments.getFragment(FragmentNameConst.FN_MEM);
            case 8:
                return Fragments.getFragment(FragmentNameConst.FN_CLEAN_CACHE);
            case 9:
                return Fragments.getFragment(FragmentNameConst.FN_HOST);
            case 10:
                return Fragments.getFragment(FragmentNameConst.FN_FEEDBACK);
            case 11:
                return Fragments.getFragment(FragmentNameConst.FN_RECOMMAND);
            case 12:
                return Fragments.getFragment(FragmentNameConst.FN_ABOUT);
            case 13:
                return Fragments.getFragment(FragmentNameConst.FN_SETTINGS);
            case 14:
                return Fragments.getFragment(FragmentNameConst.FN_RESTORE);
            case 15:
                return Fragments.getFragment(FragmentNameConst.FN_TERMINAL);
            case 16:
                return Fragments.getFragment(FragmentNameConst.FN_HARD_UPDATE);
            case 17:
                return Fragments.getFragment(FragmentNameConst.FN_DISKINFO);
            case 18:
                return Fragments.getFragment(FragmentNameConst.FN_FILESYSTEM);
            case 19:
                return Fragments.getFragment(FragmentNameConst.FN_BATCH_APPS);
            case 20:
                return Fragments.getFragment(FragmentNameConst.FN_FIREWALL);
            case 21:
                return Fragments.getFragment(FragmentNameConst.FN_GOOGLE);
            case 22:
                return Fragments.getFragment(FragmentNameConst.FN_REMAINED_FILES);
            default:
                return Fragments.getFragment(FragmentNameConst.FN_ABOUT);
        }
    }

    @Override
    public Fragment getIndexFragment() {
        return Fragments.getFragment(FragmentNameConst.FN_INDEX);
    }

    @Override
    public void initMenu(Menu menu) {
        menu.clear();
        actionItem = menu.add(0, MenuItemIds.MENU_SHARE, 0, R.string.short_share);
        actionItem.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        actionItem.setIcon(android.R.drawable.ic_menu_share);

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SHARE:
                Intent inShare = new Intent(this, ShareActivity.class);
                startActivity(inShare);
                break;
        }
        return true;
    }

    @Override
    public void onHomeClick() {

    }

    @Override
    public void onRecentAppClick() {

    }

}
