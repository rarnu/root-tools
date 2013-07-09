package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.utils.Config;

public class UserDetailFragment extends BaseFragment implements OnClickListener {

	Button btnLogout;
	boolean isShowingMyAccount = false;

	public UserDetailFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_user_detail_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.userdetail_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.userdetail_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		btnLogout = (Button) innerView.findViewById(R.id.btnLogout);
	}

	@Override
	public void initEvents() {
		btnLogout.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		String myUsrId = Config.getUserId(getActivity());
		String userId = getArguments().getString("user", "");
		isShowingMyAccount = myUsrId.equals(userId);
		Log.e("isShowingMyAccount", isShowingMyAccount ? "TRUE" : "FALSE");
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_user_detail;
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
		case R.id.btnLogout:
			Intent inRet = new Intent();
			inRet.putExtra("action", 1);
			getActivity().setResult(Activity.RESULT_OK, inRet);
			getActivity().finish();
			break;
		}
	}

}
