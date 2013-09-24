package com.sbbs.me.android.fragment;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseTabFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;

public class OnGithubTabFragment extends BaseTabFragment {

	public OnGithubTabFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_ongithub_tab_fragment);
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
		Bundle bn0 = new Bundle();
		bn0.putInt("repo_type", 0);
		GithubCodeTreeFragment gctf0 = new GithubCodeTreeFragment();
		gctf0.setArguments(bn0);
		listFragment.add(gctf0);

		Bundle bn1 = new Bundle();
		bn1.putInt("repo_type", 1);
		GithubCodeTreeFragment gctf1 = new GithubCodeTreeFragment();
		gctf1.setArguments(bn1);
		listFragment.add(gctf1);
	}
	
	@Override
	public void initLogic() {
		super.initLogic();
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_GITHUB, "");
	}

}
