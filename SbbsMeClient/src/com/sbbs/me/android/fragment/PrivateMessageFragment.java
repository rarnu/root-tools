package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
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
import com.sbbs.me.android.R;
import com.sbbs.me.android.ViewMessageActivity;
import com.sbbs.me.android.adapter.SbbsMePrivateUserAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeInboxUser;
import com.sbbs.me.android.database.PrivateMessageUtils;
import com.sbbs.me.android.loader.SbbsPrivateUserLoader;

public class PrivateMessageFragment extends BaseFragment implements
		OnClickListener, OnPullDownListener, OnItemClickListener,
		OnLoadCompleteListener<List<SbbsMeInboxUser>> {

	PullDownListView lvPullDown;
	TextView tvNodata;
	TextView tvLoading;
	SbbsPrivateUserLoader loader;
	SbbsMePrivateUserAdapter adapter;
	List<SbbsMeInboxUser> list;
	List<Boolean> listNewMessage;

	public PrivateMessageFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_private_message_fragment);
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

		list = new ArrayList<SbbsMeInboxUser>();
		listNewMessage = new ArrayList<Boolean>();
		adapter = new SbbsMePrivateUserAdapter(getActivity(), list,
				listNewMessage);
		lvPullDown.getListView().setAdapter(adapter);
		loader = new SbbsPrivateUserLoader(getActivity());
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
		tvNodata.setOnClickListener(this);
		lvPullDown.setOnPullDownListener(this);
		lvPullDown.getListView().setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void onResume() {
		super.onResume();
		adapter.setNewMessage(PrivateMessageUtils.getNewMessageStatus(
				getActivity(), list));
	}

	@Override
	public void initLogic() {
		lvPullDown.notifyDidLoad();
		if (SbbsMeAPI.isLogin()) {
			tvNodata.setText(R.string.no_data_refresh);
			tvLoading.setText(R.string.loading);
			tvLoading.setVisibility(View.VISIBLE);
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
		loader.startLoading();
	}

	@Override
	public void onMore() {

	}

	@Override
	public void onClick(View v) {
		tvLoading.setText(R.string.loading);
		tvLoading.setVisibility(View.VISIBLE);
		loader.setRefresh(false);
		loader.startLoading();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		SbbsMeInboxUser user = list.get(position);
		Intent inView = new Intent(getActivity(), ViewMessageActivity.class);
		inView.putExtra("id", user.UserId);
		inView.putExtra("name", user.Detail.Name);
		inView.putExtra("avatar", user.Detail.AvatarURL);
		startActivity(inView);
	}

	@Override
	public void onLoadComplete(Loader<List<SbbsMeInboxUser>> loader,
			List<SbbsMeInboxUser> data) {

		if (data != null) {
			list.clear();
			list.addAll(data);
		}
		if (getActivity() != null) {
			adapter.setNewList(list);
			adapter.setNewMessage(PrivateMessageUtils.getNewMessageStatus(
					getActivity(), list));
			lvPullDown.getListView().setSelected(false);
			lvPullDown.notifyDidRefresh();
			lvPullDown.notifyDidMore();
			if (!((SbbsPrivateUserLoader) loader).isRefresh()) {
				((SbbsPrivateUserLoader) loader).setRefresh(true);
				loader.startLoading();
			} else {
				tvNodata.setEnabled(true);
				tvNodata.setVisibility(list.size() == 0 ? View.VISIBLE
						: View.GONE);
				tvLoading.setVisibility(View.GONE);
			}
		}

	}

}
