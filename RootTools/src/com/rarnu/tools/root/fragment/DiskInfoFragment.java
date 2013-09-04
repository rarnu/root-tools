package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ListView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DiskInfoAdapter;
import com.rarnu.tools.root.common.DiskInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.DiskInfoLoader;

public class DiskInfoFragment extends BaseFragment implements
		OnLoadCompleteListener<List<DiskInfo>> {

	ListView lvDiskInfo;
	MenuItem miRefresh;
	List<DiskInfo> list;
	DiskInfoAdapter adapter;
	DataProgressBar progressDisk;
	DiskInfoLoader loader;

	@Override
	public int getBarTitle() {
		return R.string.func_diskinfo;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.func_diskinfo_with_path;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvDiskInfo = (ListView) innerView.findViewById(R.id.lvDiskInfo);
		progressDisk = (DataProgressBar) innerView
				.findViewById(R.id.progressDisk);
		loader = new DiskInfoLoader(getActivity());
		list = new ArrayList<DiskInfo>();
		adapter = new DiskInfoAdapter(getActivity(), list);
		lvDiskInfo.setAdapter(adapter);
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		loadDiskInfo();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.layout_diskinfo;
	}

	@Override
	public String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	public void initMenu(Menu menu) {
		miRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 99, R.string.refresh);
		miRefresh.setIcon(android.R.drawable.ic_menu_revert);
		miRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_REFRESH:
			loadDiskInfo();
			break;
		}
		return true;
	}

	private void loadDiskInfo() {

		progressDisk.setAppName(getString(R.string.loading));
		progressDisk.setVisibility(View.VISIBLE);
		loader.startLoading();

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onLoadComplete(Loader<List<DiskInfo>> loader,
			List<DiskInfo> data) {
		list.clear();
		if (data != null) {
			list.addAll(data);
		}
		adapter.setNewList(list);
		progressDisk.setVisibility(View.GONE);
	}

}
