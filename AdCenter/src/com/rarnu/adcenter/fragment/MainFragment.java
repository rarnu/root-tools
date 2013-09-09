package com.rarnu.adcenter.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;

import com.rarnu.adcenter.AdDetailActivity;
import com.rarnu.adcenter.Global;
import com.rarnu.adcenter.R;
import com.rarnu.adcenter.adapter.AdItemAdapter;
import com.rarnu.adcenter.classes.AdItem;
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
		return getString(R.string.app_name) + " - "
				+ getCurrentTitle(getArguments().getInt("area"));
	}

	@Override
	public void initComponents() {
		gvAd = (GridView) innerView.findViewById(R.id.gvAd);
		list = new ArrayList<AdItem>();
		listQuested = new ArrayList<Boolean>();
		int itemHeight = UIUtils.getWidth() / 4;
		adapter = new AdItemAdapter(getActivity(), list, listQuested,
				itemHeight);
		gvAd.setAdapter(adapter);
		loader = new AdLoader(getActivity());
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
		gvAd.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {
		loader.setData(Global.MAC_ADDRESS, 1, 50, 0, "");
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

	}

	@Override
	public void onGetNewArguments(Bundle bn) {
		setTitle(getCurrentTitle(bn.getInt("area")));
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
		if (data != null) {
			list.clear();
			list.addAll(data);
			listQuested = AdUtils.getListQuestedState(getActivity(), list);
		}
		if (getActivity() != null) {
			AdUtils.saveAds(getActivity(), list);
			adapter.setNewList(list);
			adapter.setNewQuestedList(listQuested);
		}

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		AdItem item = list.get(position);
		Intent inDetail = new Intent(getActivity(), AdDetailActivity.class);
		inDetail.putExtra("item", item);
		startActivity(inDetail);

	}

}
