package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;

public class SelectLoginFragment extends BaseFragment implements
		OnClickListener {

	RelativeLayout btnGoogle, btnGithub, btnWeibo;
	ImageView ivCloseDialog;

	public SelectLoginFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_select_login_fragment);
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
		ivCloseDialog = (ImageView) innerView.findViewById(R.id.ivCloseDialog);
	}

	@Override
	public void initEvents() {
		btnGoogle.setOnClickListener(this);
		btnGithub.setOnClickListener(this);
		btnWeibo.setOnClickListener(this);
		ivCloseDialog.setOnClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_select_login;
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
		Intent inRet = new Intent();
		switch (v.getId()) {
		case R.id.btnGoogle:
			inRet.putExtra("type", 0);
			break;
		case R.id.btnGithub:
			inRet.putExtra("type", 1);
			break;
		case R.id.btnWeibo:
			inRet.putExtra("type", 2);
			break;
		case R.id.ivCloseDialog:
			getActivity().finish();
			return;
		}
		getActivity().setResult(Activity.RESULT_OK, inRet);
		getActivity().finish();

	}
}
