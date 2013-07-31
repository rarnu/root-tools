package com.sbbs.me.android.fragment;

import java.io.Serializable;
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
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.ArticleActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.UserDetailActivity;
import com.sbbs.me.android.adapter.SbbsMeMessageAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.api.SbbsMeMessage;
import com.sbbs.me.android.loader.SbbsMessageLoader;

public class RecentFragment extends BaseFragment implements OnPullDownListener,
		OnItemClickListener, OnLoadCompleteListener<List<SbbsMeMessage>>,
		OnClickListener {

	SbbsMessageLoader loader;
	PullDownListView lvPullDown;
	TextView tvLoading;
	SbbsMeMessageAdapter adapter;
	List<SbbsMeMessage> listMessage;
	TextView tvNodata;

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
		tvNodata = (TextView) innerView.findViewById(R.id.tvNodata);
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
		lvPullDown.getListView().setOverScrollMode(View.OVER_SCROLL_NEVER);
	}

	@Override
	public void initEvents() {
		lvPullDown.getListView().setOnItemClickListener(this);
		tvNodata.setOnClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {

		if (!SbbsMeAPI.isLogin()) {
			lvPullDown.setEnabled(false);
			lvPullDown.getListView().setEnabled(false);
			tvLoading.setText(R.string.not_login);
			tvLoading.setVisibility(View.VISIBLE);
			tvNodata.setText(R.string.no_data_cannot_refresh);
			tvNodata.setVisibility(View.VISIBLE);
		} else if (listMessage.size() == 0) {
			tvLoading.setVisibility(View.VISIBLE);
			loader.startLoading();
		}
		lvPullDown.notifyDidLoad();
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_RECENT, "");
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

			startActivity(new Intent(getActivity(), ArticleActivity.class)
					.putExtra("articleId", message.postId).putExtra("item",
							(Serializable) null));

		} else {
			startActivity(new Intent(getActivity(), UserDetailActivity.class)
					.putExtra("user", message.userId));
		}
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_RECENT_CLICK, "");
	}

	@Override
	public void onLoadComplete(Loader<List<SbbsMeMessage>> loader,
			List<SbbsMeMessage> data) {
		listMessage.clear();
		if (data != null) {
			listMessage.addAll(data);
		}
		if (getActivity() != null) {
			tvNodata.setEnabled(true);
			tvNodata.setVisibility(listMessage.size() == 0 ? View.VISIBLE
					: View.GONE);
			tvNodata.setText(R.string.no_data_refresh);
			adapter.setNewList(listMessage);
			tvLoading.setVisibility(View.GONE);
			lvPullDown.notifyDidRefresh();
		}
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvNodata:
			tvNodata.setEnabled(false);
			tvLoading.setVisibility(View.VISIBLE);
			loader.startLoading();
			break;
		}
	}

}
