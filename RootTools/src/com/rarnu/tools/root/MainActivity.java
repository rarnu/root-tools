package com.rarnu.tools.root;

import android.app.Fragment;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Bitmap.CompressFormat;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ShareActionProvider;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseMainActivity;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.utils.*;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.NetworkUtils;

import java.io.File;

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

    private Intent createShareIntent() {

        String bmpName = DirHelper.ROOT_DIR + "icon.png";
        File fIcon = new File(bmpName);
        if (!fIcon.exists()) {
            Bitmap bmp = BitmapFactory.decodeResource(getResources(), R.drawable.icon);
            ImageUtils.saveBitmapToFile(bmp, bmpName, CompressFormat.PNG);
        }

        Intent shareIntent = new Intent(Intent.ACTION_SEND);
        shareIntent.setType("image/*");
        Uri uri = Uri.fromFile(fIcon);
        shareIntent.putExtra(Intent.EXTRA_STREAM, uri);
        shareIntent.putExtra(Intent.EXTRA_TEXT, getString(R.string.share_body));
        shareIntent.putExtra(Intent.EXTRA_SUBJECT, getString(R.string.share_title));
        return shareIntent;
    }

    private void initConfig() {
        GlobalInstance.isFirstStart = RTConfig.getFirstStart(this);
        GlobalInstance.allowDeleteLevel0 = RTConfig.getAllowDeleteLevel0(this);
        GlobalInstance.alsoDeleteData = RTConfig.getAlsoDeleteData(this);
        GlobalInstance.backupBeforeDelete = RTConfig.getBackupBeforeDelete(this);
        GlobalInstance.overrideBackuped = RTConfig.getOverrideBackuped(this);
        GlobalInstance.reinstallApk = RTConfig.getReinstallApk(this);
        GlobalInstance.killProcessBeforeClean = RTConfig.getKillProcessBeforeClean(this);
        GlobalInstance.nameServer = RTConfig.getNameServer(this);
        GlobalInstance.backupPath = RTConfig.getBackupPath(this);
    }

    private void loadExcludeListT() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                MemorySpecialList.loadExcludeList();
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
        RootUtils.mountRW();
        loadExcludeListT();
        loadCustomPackageListT();
        initConfig();
        if (GlobalInstance.isFirstStart) {

            GlobalInstance.isFirstStart = false;
            RTConfig.setFirstStart(this, GlobalInstance.isFirstStart);
        }

        loadNetworkStatus();
        getUpdateInfo();

    }

    @Override
    public String getBarTitle() {
        return getString(R.string.app_name);
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
                return Fragments.getFragment(FragmentNameConst.FN_BUSYBOX);
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
        actionItem = menu.add(0, MenuItemIds.MENU_ID_SHARE, 0, R.string.short_share);
        actionItem.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        actionItem.setIcon(android.R.drawable.ic_menu_share);
        ShareActionProvider actionProvider = new ShareActionProvider(this);
        actionItem.setActionProvider(actionProvider);
        actionProvider.setShareHistoryFileName(ShareActionProvider.DEFAULT_SHARE_HISTORY_FILE_NAME);
        actionProvider.setShareIntent(createShareIntent());

    }

    @Override
    public void onHomeClick() {

    }

    @Override
    public void onRecentAppClick() {

    }

}
