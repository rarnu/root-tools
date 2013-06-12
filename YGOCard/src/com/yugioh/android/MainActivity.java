package com.yugioh.android;

import android.app.Fragment;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.view.KeyEvent;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.base.inner.InnerFragment;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.UIUtils;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.database.YugiohDatabase;
import com.yugioh.android.fragments.DeckFragment;
import com.yugioh.android.fragments.DuelToolFragment;
import com.yugioh.android.fragments.LeftMenuFragment;
import com.yugioh.android.fragments.LimitFragment;
import com.yugioh.android.fragments.MainFragment;
import com.yugioh.android.fragments.NewCardFragment;
import com.yugioh.android.fragments.RightMenuFragment;
import com.yugioh.android.intf.IMainIntf;
import com.yugioh.android.utils.ResourceUtils;

public class MainActivity extends BaseSlidingActivity implements IMainIntf {

	int currentPage = 0;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
		ResourceUtils.init(this);
		super.onCreate(savedInstanceState);
		registerReceiver(receiverClose, filterClose);

		if (!YugiohDatabase.isDatabaseFileExists()) {
			UpdateInfo updateInfo = new UpdateInfo();
			updateInfo.setUpdateApk(0);
			updateInfo.setUpdateData(1);
			Intent inUpdate = new Intent(this, UpdateActivity.class);
			inUpdate.putExtra("update", updateInfo);
			startActivity(inUpdate);
			finish();
		}
	}

	@Override
	protected void onDestroy() {
		unregisterReceiver(receiverClose);
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
				getFragmentManager()
						.beginTransaction()
						.replace(R.id.fReplacement, f,
								((InnerFragment) f).getTagText()).commit();
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
			// DECK
			f = new DeckFragment();
			break;
		case 4:
			// DUEL TOOL
			f = new DuelToolFragment();
			break;
		}
		return f;
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (currentPage != 0 && keyCode == KeyEvent.KEYCODE_BACK) {
			switchPage(0, false);
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	public static final String ACTION_CLOSE_MAIN = "com.yugioh.android.close.main";

	public class CloseReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			MainActivity.this.finish();
		}
	}

	public CloseReceiver receiverClose = new CloseReceiver();
	public IntentFilter filterClose = new IntentFilter(ACTION_CLOSE_MAIN);

}
