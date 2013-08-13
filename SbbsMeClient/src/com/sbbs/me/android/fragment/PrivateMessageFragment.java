package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
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
import com.sbbs.me.android.adapter.SbbsMePrivateMessageAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeUserLite;
import com.sbbs.me.android.database.PrivateMessageUtils;
import com.sbbs.me.android.loader.SbbsPrivateMessageLoader;

public class PrivateMessageFragment extends BaseFragment implements
		OnClickListener, OnPullDownListener, OnItemClickListener,
		OnLoadCompleteListener<List<SbbsMeUserLite>> {

	PullDownListView lvPullDown;
	TextView tvNodata;
	TextView tvLoading;
	SbbsPrivateMessageLoader loader;
	SbbsMePrivateMessageAdapter adapter;
	List<SbbsMeUserLite> list;

	public PrivateMessageFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_private_message_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.message;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.message;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvPullDown = (PullDownListView) innerView.findViewById(R.id.lvPullDown);
		tvNodata = (TextView) innerView.findViewById(R.id.tvNodata);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);

		list = new ArrayList<SbbsMeUserLite>();
		adapter = new SbbsMePrivateMessageAdapter(getActivity(), list);
		lvPullDown.getListView().setAdapter(adapter);
		loader = new SbbsPrivateMessageLoader(getActivity());
		lvPullDown.enableAutoFetchMore(false, 0);
		lvPullDown.showAutoFetchMore(false);

		int devide = UIUtils.dipToPx(8);
		lvPullDown.getListView().setDivider(null);
		lvPullDown.getListView().setDividerHeight(devide);
		lvPullDown.getListView().setPadding(devide, devide, devide, devide);
		lvPullDown.getListView().setSelector(R.color.transparent);
		lvPullDown.getListView().setOverScrollMode(View.OVER_SCROLL_NEVER);

	}

	@Override
	public void initEvents() {
		tvNodata.setOnClickListener(this);
		lvPullDown.setOnPullDownListener(this);
		lvPullDown.getListView().setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		lvPullDown.notifyDidLoad();
		if (SbbsMeAPI.isLogin()) {
			tvNodata.setText(R.string.no_data_refresh);
			tvLoading.setText(R.string.loading);
			tvLoading.setVisibility(View.VISIBLE);
			loader.setQuery(
					PrivateMessageUtils.getLastMessageId(getActivity()), 1, 100);
			loader.setRefresh(false);
			loader.startLoading();
		} else {
			tvNodata.setText(R.string.no_data_cannot_refresh);
			tvNodata.setVisibility(View.VISIBLE);
			tvLoading.setText(R.string.not_login);
			tvLoading.setVisibility(View.VISIBLE);
		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_private_message;
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
		loader.setQuery(PrivateMessageUtils.getLastMessageId(getActivity()), 1,
				100);
		loader.startLoading();
	}

	@Override
	public void onMore() {

	}

	@Override
	public void onClick(View v) {


	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {

	}

	@Override
	public void onLoadComplete(Loader<List<SbbsMeUserLite>> loader,
			List<SbbsMeUserLite> data) {
		list.clear();
		if (data != null) {
			list.addAll(data);
		}
		if (getActivity() != null) {
			adapter.setNewList(list);
			lvPullDown.notifyDidRefresh();
			lvPullDown.notifyDidMore();
			if (!((SbbsPrivateMessageLoader) loader).isRefresh()) {
				((SbbsPrivateMessageLoader) loader).setRefresh(true);
				loader.startLoading();
			} else {
				tvNodata.setEnabled(true);
				tvNodata.setVisibility(Global.listArticle.size() == 0 ? View.VISIBLE
						: View.GONE);
				tvLoading.setVisibility(View.GONE);
			}
		}

	}

}
