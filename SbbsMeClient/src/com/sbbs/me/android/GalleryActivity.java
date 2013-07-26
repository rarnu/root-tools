package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;
import android.view.KeyEvent;

import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.devlib.base.BaseFragment;
import com.sbbs.me.android.fragment.GalleryFragment;

public class GalleryActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putBoolean("select_mode",
				getIntent().getBooleanExtra("select_mode", false));
		GalleryFragment gf = new GalleryFragment();
		gf.setArguments(bn);
		return gf;
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			BaseFragment bf = (BaseFragment) getFragmentManager()
					.findFragmentByTag(getString(R.tag.tag_gallery_fragment));
			Bundle bn = bf.getFragmentState();
			if (bn != null) {
				boolean em = bn.getBoolean("edit_mode", false);
				if (em) {
					bf.setNewArguments(null);
					return true;
				}
			}
		}
		return super.onKeyDown(keyCode, event);
	}
}
