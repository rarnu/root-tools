package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Environment;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeUser;
import com.sbbs.me.android.loader.SbbsUserLoader;
import com.sbbs.me.android.utils.Config;

public class UserDetailFragment extends BaseFragment implements
		OnClickListener, OnLoadCompleteListener<SbbsMeUser> {

	Button btnLogout;
	boolean isShowingMyAccount = false;
	int accType = 0;

	ImageView ivHead;
	TextView tvUserName, tvAccountTypeValue, tvLoading;
	SbbsUserLoader loader;

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
		ivHead = (ImageView) innerView.findViewById(R.id.ivHead);
		tvUserName = (TextView) innerView.findViewById(R.id.tvUserName);
		tvAccountTypeValue = (TextView) innerView
				.findViewById(R.id.tvAccountTypeValue);
		btnLogout = (Button) innerView.findViewById(R.id.btnLogout);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);

		loader = new SbbsUserLoader(getActivity());
	}

	@Override
	public void initEvents() {
		btnLogout.setOnClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		String myUsrId = Config.getUserId(getActivity());
		String userId = getArguments().getString("user", "");
		accType = Config.getAccountType(getActivity());
		isShowingMyAccount = myUsrId.equals(userId);
		btnLogout.setVisibility(isShowingMyAccount ? View.VISIBLE : View.GONE);

		tvLoading.setVisibility(View.VISIBLE);
		loader.setUserId(myUsrId, userId);
		loader.startLoading();

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

	@Override
	public void onLoadComplete(Loader<SbbsMeUser> loader, SbbsMeUser data) {
		tvLoading.setVisibility(View.GONE);
		if (data != null) {

			Log.e("onLoadComplete",
					String.format("follow:%d", data.followStatus));

			tvUserName.setText(data.Name);
			tvAccountTypeValue.setText(data.Type);

			String headLocalPath = Environment.getExternalStorageDirectory()
					.getPath() + "/.sbbs/";
			String headLocalName = data.Id + ".jpg";
			DownloadUtils.downloadFileT(getActivity(), ivHead, data.AvatarURL,
					headLocalPath, headLocalName, null);
		} else {
			Toast.makeText(getActivity(), R.string.user_error,
					Toast.LENGTH_LONG).show();
			getActivity().finish();
		}
	}

}
