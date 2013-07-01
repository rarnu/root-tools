package com.sbbs.me.android.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.utils.OAuthUtils;

public class SelectLoginFragment extends BaseFragment implements OnClickListener {

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
		OAuthUtils.sendSinaOauth(getActivity());
	}

}
