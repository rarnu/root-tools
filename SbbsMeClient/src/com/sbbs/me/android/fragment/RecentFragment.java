package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.UserDetailActivity;
import com.sbbs.me.android.adapter.SbbsMeMessageAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeMessage;
import com.sbbs.me.android.loader.SbbsMessageLoader;

public class RecentFragment extends BaseFragment implements OnPullDownListener,
		OnItemClickListener, OnLoadCompleteListener<List<SbbsMeMessage>> {

	SbbsMessageLoader loader;
	PullDownListView lvPullDown;
	TextView tvLoading;
	SbbsMeMessageAdapter adapter;
	List<SbbsMeMessage> listMessage;

	public RecentFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_recent_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_recent;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_recent;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvPullDown = (PullDownListView) innerView.findViewById(R.id.lvPullDown);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		if (listMessage == null) {
			listMessage = new ArrayList<SbbsMeMessage>();
		}
		adapter = new SbbsMeMessageAdapter(getActivity(), listMessage);
		lvPullDown.getListView().setAdapter(adapter);
		loader = new SbbsMessageLoader(getActivity());
		lvPullDown.enableAutoFetchMore(true, 1);
		lvPullDown.setOnPullDownListener(this);

		int devide = UIUtils.dipToPx(8);
		lvPullDown.getListView().setDivider(null);
		lvPullDown.getListView().setDividerHeight(devide);

		lvPullDown.getListView().setPadding(devide, devide, devide, devide);
	}

	@Override
	public void initEvents() {
		lvPullDown.getListView().setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {

		if (!SbbsMeAPI.isLogin()) {
			lvPullDown.setEnabled(false);
			lvPullDown.getListView().setEnabled(false);
			tvLoading.setText(R.string.not_login);
			tvLoading.setVisibility(View.VISIBLE);

		} else if (listMessage.size() == 0) {
			tvLoading.setVisibility(View.VISIBLE);
			loader.startLoading();
		}
		lvPullDown.notifyDidLoad();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_recent;
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
	public void onRefresh() {
		loader.startLoading();
	}

	private Handler hDid = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				lvPullDown.notifyDidMore();
			}
			super.handleMessage(msg);
		};
	};

	@Override
	public void onMore() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				try {
					Thread.sleep(500);
				} catch (Exception e) {

				}
				hDid.sendEmptyMessage(1);
			}
		}).start();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		SbbsMeMessage message = listMessage.get(position);
		if (message.actionType == 0) {
			// TODO: view article
		} else {
			startActivity(new Intent(getActivity(), UserDetailActivity.class)
					.putExtra("user", message.userId));
		}
	}

	@Override
	public void onLoadComplete(Loader<List<SbbsMeMessage>> loader,
			List<SbbsMeMessage> data) {
		listMessage.clear();
		if (data != null) {
			listMessage.addAll(data);
		}

		adapter.setNewList(listMessage);
		tvLoading.setVisibility(View.GONE);
		lvPullDown.notifyDidRefresh();
	}

}
