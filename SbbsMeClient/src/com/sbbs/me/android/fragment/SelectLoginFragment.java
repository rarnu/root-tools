package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeSinaUser;
import com.sbbs.me.android.utils.SinaOAuth;
import com.sbbs.me.android.utils.SinaOAuth.SinaUserCallback;

public class SelectLoginFragment extends BaseFragment implements
		OnClickListener, SinaUserCallback {

	Button btnWeibo;

	public SelectLoginFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_select_login_fragment);
	}

	@Override
	public int getBarTitle() {
		return 0;
	}

	@Override
	public int getBarTitleWithPath() {
		return 0;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		btnWeibo = (Button) innerView.findViewById(R.id.btnWeibo);

	}

	@Override
	public void initEvents() {
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
		SinaOAuth auth = new SinaOAuth(getActivity(), this);
		auth.sendSinaOauth();
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
