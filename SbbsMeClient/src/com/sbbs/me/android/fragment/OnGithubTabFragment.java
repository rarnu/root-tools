package com.sbbs.me.android.fragment;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseTabFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;

public class OnGithubTabFragment extends BaseTabFragment {

	public OnGithubTabFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_ongithub_tab_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_ongithub;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_ongithub;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void initFragmentList(List<Fragment> listFragment) {
		listFragment.add(new GithubCodeTreeFragment((byte)0, null));
		listFragment.add(new GithubCodeTreeFragment((byte)1, null));
	}

}
