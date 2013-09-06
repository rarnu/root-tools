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
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.TagArticleListActivity;
import com.sbbs.me.android.adapter.SbbsMeTagAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.api.SbbsMeTag;
import com.sbbs.me.android.loader.SbbsTagLoader;

public class HotTagsFragment extends BaseFragment implements
		OnPullDownListener, OnItemClickListener,
		OnLoadCompleteListener<List<SbbsMeTag>>, OnClickListener {

	PullDownListView lvPullDown;
	SbbsTagLoader loader;
	SbbsMeTagAdapter adapter;
	TextView tvLoading;
	TextView tvNodata;

	public HotTagsFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_hottags_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_hottags;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_hottags;
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

		if (Global.listTags == null) {
			Global.listTags = new ArrayList<SbbsMeTag>();
		}
		adapter = new SbbsMeTagAdapter(getActivity(), Global.listTags);
		lvPullDown.getListView().setAdapter(adapter);
		loader = new SbbsTagLoader(getActivity());
		lvPullDown.enableAutoFetchMore(false, 0);
		lvPullDown.showAutoFetchMore(false);

		int devide = UIUtils.dipToPx(8);
		lvPullDown.getListView().setDivider(null);
		lvPullDown.getListView().setDividerHeight(devide);
		lvPullDown.getListView().setPadding(devide, devide, devide, devide);
		lvPullDown.getListView().setSelector(R.color.transparent);
		lvPullDown.getListView().setOverScrollMode(View.OVER_SCROLL_NEVER);
		lvPullDown.getListView().setFocusableInTouchMode(false);
	}

	@Override
	public void initEvents() {
		lvPullDown.setOnPullDownListener(this);
		lvPullDown.getListView().setOnItemClickListener(this);
		tvNodata.setOnClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		if (Global.listTags.size() == 0) {
			tvLoading.setVisibility(View.VISIBLE);
			loader.setRefresh(false);
			loader.startLoading();
		}
		lvPullDown.notifyDidLoad();
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_HOT_TAGS, "");
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_hottags;
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
		loader.setRefresh(true);
		loader.startLoading();

	}

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
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		SbbsMeTag item = Global.listTags.get(position);
		startActivity(new Intent(getActivity(), TagArticleListActivity.class)
				.putExtra("item", item));
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_HOT_TAGS_CLICK, "");
	}

	@Override
	public void onLoadComplete(Loader<List<SbbsMeTag>> loader,
			List<SbbsMeTag> data) {
		Global.listTags.clear();
		if (data != null) {
			Global.listTags.addAll(data);
		}
		if (getActivity() != null) {
			adapter.setNewList(Global.listTags);
			lvPullDown.getListView().setSelected(false);
			lvPullDown.notifyDidRefresh();

			if (!((SbbsTagLoader) loader).isRefresh()) {
				((SbbsTagLoader) loader).setRefresh(true);
				loader.startLoading();
			} else {
				tvNodata.setEnabled(true);
				tvNodata.setVisibility(Global.listTags.size() == 0 ? View.VISIBLE
						: View.GONE);
				tvLoading.setVisibility(View.GONE);
			}
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
