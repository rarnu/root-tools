package com.sbbs.me.android.fragment;

import java.util.List;

import android.content.ComponentName;
import android.content.Intent;
import android.content.pm.ResolveInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;
import android.widget.ImageView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.ShareAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;

public class ShareFragment extends BaseFragment implements OnClickListener,
		OnItemClickListener {

	ImageView ivCloseDialog;
	GridView lvShare;
	String body;
	ShareAdapter adapter;
	List<ResolveInfo> listInfo;

	public ShareFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_share_fragment);
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
		ivCloseDialog = (ImageView) innerView.findViewById(R.id.ivCloseDialog);
		lvShare = (GridView) innerView.findViewById(R.id.lvShare);
	}

	@Override
	public void initEvents() {
		ivCloseDialog.setOnClickListener(this);
		lvShare.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {
		body = getArguments().getString("body");
		getShareAppsT();
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_ARTICLE_SHARE, "");
	}

	private Handler hShowApp = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				adapter = new ShareAdapter(getActivity(), listInfo);
				lvShare.setAdapter(adapter);
			}
			super.handleMessage(msg);
		};
	};

	private void getShareAppsT() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				Intent shareIntent = new Intent(Intent.ACTION_SEND);
				shareIntent.setType("image/*");
				listInfo = getActivity().getPackageManager()
						.queryIntentActivities(shareIntent, 0);
				hShowApp.sendEmptyMessage(1);

			}
		}).start();
	}

	// private void share() {
	// Intent shareIntent = new Intent(Intent.ACTION_SEND);
	// shareIntent.setType("image/*");
	// shareIntent.putExtra(Intent.EXTRA_TEXT, body);
	// startActivity(shareIntent);
	// }

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_share;
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
		case R.id.ivCloseDialog:
			getActivity().finish();
			break;
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		ResolveInfo info = listInfo.get(position);
		share(info.activityInfo.packageName, info.activityInfo.name);
	}

	private void share(String pkgName, String name) {
		Intent shareIntent = new Intent(Intent.ACTION_SEND);
		shareIntent.setType("image/*");
		shareIntent.putExtra(Intent.EXTRA_TEXT, body);
		shareIntent.setComponent(new ComponentName(pkgName, name));
		startActivity(shareIntent);
	}

}
