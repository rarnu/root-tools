package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;

public class FileSystemFragment extends BaseFragment implements
		OnQueryTextListener, OnItemClickListener {

	MenuItem itemRefresh;
	MenuItem itemSearch;
	MenuItem itemUp;

	ListView lvFiles;
	DataProgressBar progressFiles;

	@Override
	public int getBarTitle() {
		return R.string.func_filesystem;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.func_filesystem_with_path;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvFiles = (ListView) innerView.findViewById(R.id.lvFiles);
		progressFiles = (DataProgressBar) innerView
				.findViewById(R.id.progressFiles);

	}

	@Override
	public void initEvents() {
		lvFiles.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.layout_filesystem;
	}

	@Override
	public String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	public void initMenu(Menu menu) {
		itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 98, R.string.search);
		itemSearch.setIcon(android.R.drawable.ic_menu_search);
		itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		SearchView sv = new SearchView(getActivity());
		sv.setOnQueryTextListener(this);
		itemSearch.setActionView(sv);

		itemUp = menu.add(0, MenuItemIds.MENU_UPLEVEL, 99, R.string.uplevel);
		itemUp.setIcon(android.R.drawable.ic_menu_upload);
		itemUp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

		itemRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 100,
				R.string.refresh);
		itemRefresh.setIcon(android.R.drawable.ic_menu_revert);
		itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_UPLEVEL:
			// TODO: up level
			break;
		case MenuItemIds.MENU_REFRESH:
			// TODO: refresh
			
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
	public boolean onQueryTextSubmit(String query) {
		return false;
	}

	@Override
	public boolean onQueryTextChange(String newText) {
		// adapter.getFilter().filter(newText);
		return false;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {

	}

}
