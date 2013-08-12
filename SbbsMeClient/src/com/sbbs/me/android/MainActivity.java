package com.sbbs.me.android;

import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.base.inner.InnerFragment;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.fragment.ArchievementFragment;
import com.sbbs.me.android.fragment.HotTagsFragment;
import com.sbbs.me.android.fragment.LeftMenuFragment;
import com.sbbs.me.android.fragment.MainFragment;
import com.sbbs.me.android.fragment.OnGithubTabFragment;
import com.sbbs.me.android.fragment.PostNewFragment;
import com.sbbs.me.android.fragment.RecentFragment;
import com.sbbs.me.android.service.MessageService;

public class MainActivity extends BaseSlidingActivity implements IMainIntf {

	int currentPage = 0;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
		ResourceUtils.init(this);
		Global.autoRefreshTag = true;
		super.onCreate(savedInstanceState);
		startService(new Intent(this, MessageService.class));
	}

	@Override
	protected void onDestroy() {
		SbbsMeAPI.logout();
		Global.releaseAll();
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
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (currentPage != 0 && keyCode == KeyEvent.KEYCODE_BACK) {
			switchPage(0, false);
			return true;
		}
		return super.onKeyDown(keyCode, event);
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
			f = getFragmentManager().findFragmentByTag(
					getString(R.tag.tag_main_fragment));
			if (f == null) {
				Log.e("getCurrentFragment", "new MainFragment()");
				f = new MainFragment();
			}
			break;
		case 1:
			// post new
			f = getFragmentManager().findFragmentByTag(
					getString(R.tag.tag_postnew_fragment));
			if (f == null) {
				Log.e("getCurrentFragment", "new PostNewFragment()");
				f = new PostNewFragment();
			}
			break;
		case 2:
			// recent
			f = getFragmentManager().findFragmentByTag(
					getString(R.tag.tag_recent_fragment));
			if (f == null) {
				Log.e("getCurrentFragment", "new RecentFragment()");
				f = new RecentFragment();
			}
			break;
		case 3:
			// hot tags
			f = getFragmentManager().findFragmentByTag(
					getString(R.tag.tag_hottags_fragment));
			if (f == null) {
				Log.e("getCurrentFragment", "new HotTagsFragment()");
				f = new HotTagsFragment();
			}
			break;
		case 4:
			// on github
			f = getFragmentManager().findFragmentByTag(
					getString(R.tag.tag_ongithub_tab_fragment));
			if (f == null) {
				Log.e("getCurrentFragment", "new OnGithubTabFragment()");
				f = new OnGithubTabFragment();
			}
			break;
		case 5:
			// archievement
			f = getFragmentManager().findFragmentByTag(
					getString(R.tag.tag_archievement_fragment));
			if (f == null) {
				Log.e("getCurrentFragment", "new ArchievementFragment()");
				f = new ArchievementFragment();
			}
			break;
		}
		return f;
	}

}
