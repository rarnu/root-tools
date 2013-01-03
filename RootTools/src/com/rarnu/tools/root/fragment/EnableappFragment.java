package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.EnableappAdapter;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.base.MenuItemIds;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.loader.EnableappLoader;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ComponentUtils;

public class EnableappFragment extends BaseFragment implements
		OnItemLongClickListener, OnLoadCompleteListener<List<EnableappInfo>>, OnQueryTextListener {

	ListView lvEnableApp;
	DataProgressBar progressEnableapp;
	TextView tvOperateHint;

	boolean enableappLoading = false;
	EnableappAdapter enableappAdapter;
	List<EnableappInfo> listEnableappAll = new ArrayList<EnableappInfo>();

	EnableappLoader loader = null;

	@Override
	protected int getBarTitle() {
		return R.string.func2_title;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.func2_title_with_path;
	}

	@Override
	protected void initComponents() {

		lvEnableApp = (ListView) innerView.findViewById(R.id.lvEnableApp);
		progressEnableapp = (DataProgressBar) innerView
				.findViewById(R.id.progressEnableapp);
		tvOperateHint = (TextView) innerView.findViewById(R.id.tvOperateHint);

		enableappAdapter = new EnableappAdapter(getActivity()
				.getLayoutInflater(), listEnableappAll);
		lvEnableApp.setAdapter(enableappAdapter);

		lvEnableApp.setOnItemLongClickListener(this);

		loader = new EnableappLoader(getActivity());
		loader.registerListener(0, this);
		doStartLoad();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_enableapp;
	}

	@Override
	protected void initMenu(Menu menu) {
		MenuItem itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 98,
				R.string.search);
		itemSearch.setIcon(android.R.drawable.ic_menu_search);
		itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		SearchView sv = new SearchView(getActivity());
		sv.setOnQueryTextListener(this);
		itemSearch.setActionView(sv);
		
		MenuItem itemRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 99,
				R.string.refresh);
		itemRefresh.setIcon(android.R.drawable.ic_menu_revert);
		itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_REFRESH:
			doStartLoad();
			break;
		}
		return true;
	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view,
			int position, long id) {
		final EnableappInfo info = (EnableappInfo) lvEnableApp
				.getItemAtPosition(position);

		if (info.type == 0) {
			boolean ret = false;
			if (info.enabled) {
				ret = ComponentUtils.doDisableApplication(info);
				if (ret) {
					info.enabled = false;
				}
			} else {
				ret = ComponentUtils.doEnableApplication(info);
				if (ret) {
					info.enabled = true;
				}
			}
			if (ret) {
				enableappAdapter.sort();
				enableappAdapter.notifyDataSetChanged();
			} else {
				Toast.makeText(getActivity(),
						R.string.change_package_status_fail, Toast.LENGTH_LONG)
						.show();
			}
		} else if (info.type == 1) {

			AlertDialogEx.showAlertDialogEx(getActivity(),
					getString(R.string.func2_title),
					getString(R.string.data_app_uninstall),
					getString(R.string.ok),
					new AlertDialogEx.DialogButtonClickListener() {

						@Override
						public void onClick(View v) {
							if (ApkUtils.uninstallApk(info.info.packageName)) {
								enableappAdapter.deleteItem(info);
							} else {
								Toast.makeText(getActivity(),
										R.string.cannot_uninstall_package,
										Toast.LENGTH_LONG).show();
							}
						}
					}, getString(R.string.cancel), null);

		} else {
			Toast.makeText(getActivity(),
					R.string.cannot_change_package_status, Toast.LENGTH_LONG)
					.show();
		}

		return true;
	}

	private void doStartLoad() {
		tvOperateHint.setVisibility(View.GONE);
		progressEnableapp.setAppName(getString(R.string.loading));
		progressEnableapp.setProgress("");
		progressEnableapp.setVisibility(View.VISIBLE);
		enableappLoading = true;
		loader.startLoading();
	}

	@Override
	public void onLoadComplete(Loader<List<EnableappInfo>> loader,
			List<EnableappInfo> data) {

		listEnableappAll.clear();
		if (data != null) {
			listEnableappAll.addAll(data);
		}
		enableappAdapter.setNewList(listEnableappAll);
		progressEnableapp.setVisibility(View.GONE);
		tvOperateHint.setVisibility(View.VISIBLE);
		enableappLoading = false;

	}

	@Override
	public boolean onQueryTextSubmit(String query) {

		return false;
	}

	@Override
	public boolean onQueryTextChange(String newText) {
		if (enableappAdapter != null) {
			enableappAdapter.getFilter().filter(newText);
		}
		return false;
	}

}
