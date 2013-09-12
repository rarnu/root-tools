package com.rarnu.adcenter.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;
import android.widget.ShareActionProvider;
import android.widget.TextView;

import com.rarnu.adcenter.AdDetailActivity;
import com.rarnu.adcenter.Global;
import com.rarnu.adcenter.LoginActivity;
import com.rarnu.adcenter.R;
import com.rarnu.adcenter.adapter.AdItemAdapter;
import com.rarnu.adcenter.classes.AdItem;
import com.rarnu.adcenter.classes.UserItem;
import com.rarnu.adcenter.common.MenuIds;
import com.rarnu.adcenter.database.AdUtils;
import com.rarnu.adcenter.loader.AdLoader;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

public class MainFragment extends BaseFragment implements
		OnLoadCompleteListener<List<AdItem>>, OnItemClickListener {

	GridView gvAd;
	AdItemAdapter adapter;
	List<AdItem> list;
	List<Boolean> listQuested;
	AdLoader loader;
	MenuItem itemRefresh;
	MenuItem itemShare;
	int type = 0;
	TextView tvLoading;

	public MainFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_main);
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
		return getString(R.string.app_name) + " - " + getCurrentTitle(type);
	}

	@Override
	public void initComponents() {
		gvAd = (GridView) innerView.findViewById(R.id.gvAd);
		list = new ArrayList<AdItem>();
		listQuested = new ArrayList<Boolean>();
		int itemHeight = UIUtils.getWidth() / 3;
		adapter = new AdItemAdapter(getActivity(), list, listQuested,
				itemHeight);
		gvAd.setAdapter(adapter);
		loader = new AdLoader(getActivity());
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
		gvAd.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {
		type = getArguments().getInt("area");
		loader.setData(Global.MAC_ADDRESS, 1, 50, type, "");
		tvLoading.setVisibility(View.VISIBLE);
		loader.startLoading();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_main;
	}

	@Override
	public void onResume() {
		super.onResume();
		listQuested = AdUtils.getListQuestedState(getActivity(), list);
		adapter.setNewQuestedList(listQuested);
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		itemRefresh = menu.add(0, MenuIds.MENUID_REFRESH, 99, R.string.refresh);
		itemRefresh.setIcon(android.R.drawable.ic_menu_revert);
		itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		itemShare = menu.add(0, MenuIds.MENUID_SHARE, 100, R.string.share);
		itemShare.setIcon(android.R.drawable.ic_menu_share);
		itemShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		ShareActionProvider sap = new ShareActionProvider(getActivity());
		sap.setShareHistoryFileName(ShareActionProvider.DEFAULT_SHARE_HISTORY_FILE_NAME);
		sap.setShareIntent(getShareIntent());
		itemShare.setActionProvider(sap);
	}

	private Intent getShareIntent() {
		Intent shareIntent = new Intent(Intent.ACTION_SEND);
		shareIntent.setType("image/*");
		shareIntent.putExtra(Intent.EXTRA_TEXT, getString(R.string.share_body));
		shareIntent.putExtra(Intent.EXTRA_SUBJECT,
				getString(R.string.share_title));
		return shareIntent;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENUID_REFRESH:
			tvLoading.setVisibility(View.VISIBLE);
			loader.startLoading();
			break;
		}
		return true;
	}

	@Override
	public void onGetNewArguments(Bundle bn) {
		type = bn.getInt("area");
		setTitle(getCurrentTitle(type));
		loader.setData(Global.MAC_ADDRESS, 1, 50, type, "");
		tvLoading.setVisibility(View.VISIBLE);
		loader.startLoading();
	}

	private String getCurrentTitle(int index) {
		String[] titles = getResources()
				.getStringArray(R.array.left_menu_items);
		return titles[index];
	}

	private void setTitle(String str) {
		if (getActivity() != null) {
			getActivity().getActionBar().setTitle(
					getString(R.string.app_name) + " - " + str);
		}
	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onLoadComplete(Loader<List<AdItem>> loader, List<AdItem> data) {
		list.clear();

		if (data != null) {
			list.addAll(data);
			listQuested = AdUtils.getListQuestedState(getActivity(), list);
		}
		if (getActivity() != null) {
			AdUtils.saveAds(getActivity(), list);
			adapter.setNewList(list);
			adapter.setNewQuestedList(listQuested);
			tvLoading.setVisibility(View.GONE);
		}

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {

		UserItem user = AdUtils.queryUser(getActivity());
		if (user == null) {
			startActivity(new Intent(getActivity(), LoginActivity.class));
		} else {
			AdItem item = list.get(position);
			Intent inDetail = new Intent(getActivity(), AdDetailActivity.class);
			inDetail.putExtra("item", item);
			startActivity(inDetail);
		}
	}

}
