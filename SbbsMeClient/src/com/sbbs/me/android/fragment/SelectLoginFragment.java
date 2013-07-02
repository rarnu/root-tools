package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeSinaUser;
import com.sbbs.me.android.utils.SinaOAuth;
import com.sbbs.me.android.utils.SinaOAuth.SinaUserCallback;

public class SelectLoginFragment extends BaseFragment implements
		OnClickListener, SinaUserCallback {

	RelativeLayout btnGoogle, btnGithub, btnWeibo;

	public SelectLoginFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_select_login_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.oauth_login;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.oauth_login;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		btnGoogle = (RelativeLayout) innerView.findViewById(R.id.btnGoogle);
		btnGithub = (RelativeLayout) innerView.findViewById(R.id.btnGithub);
		btnWeibo = (RelativeLayout) innerView.findViewById(R.id.btnWeibo);

	}

	@Override
	public void initEvents() {
		btnGoogle.setOnClickListener(this);
		btnGithub.setOnClickListener(this);
		btnWeibo.setOnClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_select_login;
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
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnGoogle:
			break;
		case R.id.btnGithub:
			break;
		case R.id.btnWeibo:
			SinaOAuth auth = new SinaOAuth(getActivity(), this);
			auth.sendSinaOauth();
			break;
		}

	}

	@Override
	public void onGetSinaUser(SbbsMeSinaUser user) {
		Intent inRet = new Intent();
		inRet.putExtra("type", 2);
		inRet.putExtra("user", user);
		getActivity().setResult(Activity.RESULT_OK, inRet);
		getActivity().finish();
	}

}
