package com.rarnu.tools.root;

import java.io.File;

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

import com.rarnu.devlib.base.BaseMainActivity;
import com.rarnu.devlib.utils.ImageUtils;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.fragment.GlobalFragment;
import com.rarnu.tools.root.utils.CustomPackageUtils;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.NetworkUtils;
import com.rarnu.tools.root.utils.UpdateUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class MainActivity extends BaseMainActivity {

	MenuItem actionItem;

	private Intent createShareIntent() {

		String bmpName = DirHelper.ROOT_DIR + "icon.png";
		File fIcon = new File(bmpName);
		if (!fIcon.exists()) {
			Bitmap bmp = BitmapFactory.decodeResource(getResources(),
					R.drawable.icon);
			ImageUtils.saveBitmapToFile(bmp, bmpName, CompressFormat.PNG);
		}

		Intent shareIntent = new Intent(Intent.ACTION_SEND);
		shareIntent.setType("image/*");
		Uri uri = Uri.fromFile(fIcon);
		shareIntent.putExtra(Intent.EXTRA_STREAM, uri);
		shareIntent.putExtra(Intent.EXTRA_TEXT, getString(R.string.share_body));
		shareIntent.putExtra(Intent.EXTRA_SUBJECT,
				getString(R.string.share_title));
		return shareIntent;
	}

	private void initConfig() {
		GlobalInstance.isFirstStart = RTConfig.getFirstStart(this);
		GlobalInstance.allowDeleteLevel0 = RTConfig.getAllowDeleteLevel0(this);
		GlobalInstance.alsoDeleteData = RTConfig.getAlsoDeleteData(this);
		GlobalInstance.backupBeforeDelete = RTConfig
				.getBackupBeforeDelete(this);
		GlobalInstance.overrideBackuped = RTConfig.getOverrideBackuped(this);
		GlobalInstance.reinstallApk = RTConfig.getReinstallApk(this);
		GlobalInstance.killProcessBeforeClean = RTConfig
				.getKillProcessBeforeClean(this);
		GlobalInstance.nameServer = RTConfig.getNameServer(this);
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

	final Handler hUpdate = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				if (GlobalInstance.updateInfo != null
						&& GlobalInstance.updateInfo.result != 0) {
					UpdateUtils.showUpdateInfo(MainActivity.this);
				}
			}
			super.handleMessage(msg);
		}
	};

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
		GlobalFragment.loadFragments();

	}

	@Override
	public void releaseFragments() {
		GlobalFragment.releaseFragments();

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
			return GlobalFragment.fSysapp;
		case 2:
			return GlobalFragment.fEnableapp;
		case 3:
			return GlobalFragment.fComp;
		case 4:
			return GlobalFragment.fBusybox;
		case 5:
			return GlobalFragment.fHtcRom;
		case 6:
			return GlobalFragment.fBackup;
		case 7:
			return GlobalFragment.fMem;
		case 8:
			return GlobalFragment.fCleanCache;
		case 9:
			return GlobalFragment.fHost;
		case 10:
			return GlobalFragment.fFeedback;
		case 11:
			return GlobalFragment.fRecommand;
		case 12:
			return GlobalFragment.fAbout;
		case 13:
			return GlobalFragment.fSettings;
		case 14:
			return GlobalFragment.fRestore;
		default:
			return GlobalFragment.fIntro;
		}
	}

	@Override
	public Fragment getIndexFragment() {
		return GlobalFragment.fIndex;
	}

	@Override
	public void initMenu(Menu menu) {
		menu.clear();
		actionItem = menu.add(0, MenuItemIds.MENU_ID_SHARE, 0,
				R.string.short_share);
		actionItem.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		actionItem.setIcon(android.R.drawable.ic_menu_share);
		ShareActionProvider actionProvider = new ShareActionProvider(this);
		actionItem.setActionProvider(actionProvider);
		actionProvider
				.setShareHistoryFileName(ShareActionProvider.DEFAULT_SHARE_HISTORY_FILE_NAME);
		actionProvider.setShareIntent(createShareIntent());

	}

	@Override
	public void onHomeClick() {
		GlobalFragment.releaseFragments();
		oneTimeRun = false;
		finish();

	}

	@Override
	public void onRecentAppClick() {

	}

}
