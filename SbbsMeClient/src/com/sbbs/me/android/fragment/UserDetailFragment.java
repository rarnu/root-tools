package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.api.SbbsMeUser;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.consts.PathDefine;
import com.sbbs.me.android.dialog.SendMessageDialog;
import com.sbbs.me.android.loader.SbbsUserLoader;
import com.sbbs.me.android.utils.Config;
import com.sbbs.me.android.utils.CustomUtils;
import com.sbbs.me.android.utils.MiscUtils;

public class UserDetailFragment extends BaseFragment implements
		OnLoadCompleteListener<SbbsMeUser>, OnClickListener {

	boolean isShowingMyAccount = false;
	int accType = 0;

	ImageView ivHead;
	TextView tvUserName, tvAccountType, tvLoading;
	SbbsUserLoader loader;
	Button btnLogout;
	TextView tvRelationship;
	Button btnFollow;
	SbbsMeUser user;
	String myUsrId;
	String userId;
	RelativeLayout layLastPost;
	MenuItem miMessage;

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
		tvAccountType = (TextView) innerView.findViewById(R.id.tvAccountType);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		tvRelationship = (TextView) innerView.findViewById(R.id.tvRelationship);
		btnFollow = (Button) innerView.findViewById(R.id.btnFollow);
		layLastPost = (RelativeLayout) innerView.findViewById(R.id.layLastPost);
		btnLogout = (Button) innerView.findViewById(R.id.btnLogout);

		loader = new SbbsUserLoader(getActivity());
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
		btnFollow.setOnClickListener(this);
		btnLogout.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		myUsrId = Config.getUserId(getActivity());
		userId = getArguments().getString("user", "");
		accType = Config.getAccountType(getActivity());
		isShowingMyAccount = myUsrId.equals(userId);

		if (btnLogout != null) {
			btnLogout.setVisibility(isShowingMyAccount ? View.VISIBLE
					: View.GONE);
		}
		btnFollow.setVisibility(isShowingMyAccount ? View.GONE : View.VISIBLE);
		btnFollow.setEnabled(false);
		tvLoading.setVisibility(View.VISIBLE);
		loader.setUserId(myUsrId, userId);
		loader.startLoading();
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_USER_DETAIL, "");
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
		miMessage = menu.add(0, MenuIds.MENU_ID_MESSAGE, 99, R.string.message);
		miMessage.setIcon(MiscUtils.loadResIcon(getActivity(),
				R.drawable.ic_menu_notifications));
		miMessage.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_MESSAGE:
			if (isShowingMyAccount) {
				Toast.makeText(getActivity(),
						R.string.cannot_send_message_to_self, Toast.LENGTH_LONG)
						.show();
			} else {
				startActivity(new Intent(getActivity(), SendMessageDialog.class)
						.putExtra("user", userId));
			}
			break;
		}
		return true;
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onLoadComplete(Loader<SbbsMeUser> loader, SbbsMeUser data) {
		user = data;
		if (getActivity() != null) {
			tvLoading.setVisibility(View.GONE);
			btnFollow.setEnabled(true);
			if (data != null) {
				tvUserName.setText(data.Name);
				tvAccountType.setText(getString(R.string.user_account_type,
						data.Type));

				setFollowStatus(data.followStatus, data.Name);
				if (data.lastBlock != null) {
					CustomUtils.addBlock(getActivity(), data.lastBlock, 0, 0,
							"", layLastPost, 130000, 130000, false, null, null);
				}
				String headLocalPath = PathDefine.ROOT_PATH;
				String headLocalName = data.Id + ".jpg";
				DownloadUtils.downloadFileT(getActivity(), ivHead,
						data.AvatarURL, headLocalPath, headLocalName, null);
			} else {
				Toast.makeText(getActivity(), R.string.user_error,
						Toast.LENGTH_LONG).show();
				getActivity().finish();
			}
		}
	}

	private void setFollowStatus(int stat, String name) {
		switch (stat) {
		case 0:
			tvRelationship.setText(R.string.user_follow_0);
			btnFollow.setText(R.string.user_follow);
			break;
		case 1:
			tvRelationship.setText(getString(R.string.user_follow_1, name));
			btnFollow.setText(R.string.user_unfollow);
			break;
		case 2:
			tvRelationship.setText(getString(R.string.user_follow_2, name));
			btnFollow.setText(R.string.user_follow);
			break;
		case 3:
			tvRelationship.setText(R.string.user_follow_3);
			btnFollow.setText(R.string.user_unfollow);
			break;

		}

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnFollow:
			if (user != null) {
				userOperationT(user.followStatus == 0 || user.followStatus == 2);
			}
			break;
		case R.id.btnLogout:
			Intent inRet = new Intent();
			inRet.putExtra("action", 1);
			getActivity().setResult(Activity.RESULT_OK, inRet);
			getActivity().finish();
			break;
		}
	}

	final Handler hUserOper = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				loader.startLoading();
			}
			super.handleMessage(msg);
		};
	};

	private void userOperationT(final boolean follow) {
		btnFollow.setEnabled(false);
		tvLoading.setVisibility(View.VISIBLE);
		new Thread(new Runnable() {
			@Override
			public void run() {
				if (follow) {
					SbbsMeAPI.followUser(myUsrId, userId);
					SbbsMeAPI.writeLogT(getActivity(),
							SbbsMeLogs.LOG_FOLLOW_USER, "");
				} else {
					SbbsMeAPI.unfollowUser(myUsrId, userId);
					SbbsMeAPI.writeLogT(getActivity(),
							SbbsMeLogs.LOG_UNFOLLOW_USER, "");
				}
				hUserOper.sendEmptyMessage(1);
			}
		}).start();
	}
}
