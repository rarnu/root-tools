package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.Toast;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.SysappAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.base.MenuItemIds;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.fragmentactivity.SysappDetailActivity;
import com.rarnu.tools.root.fragmentactivity.SysappSelectApkActivity;
import com.rarnu.tools.root.loader.SysappLoader;

public class SysappFragment extends BaseFragment implements
		OnQueryTextListener, OnItemClickListener,
		OnLoadCompleteListener<List<SysappInfo>> {

	DataProgressBar progressSysapp;
	ListView lvSysApp;

	List<SysappInfo> listSysappAll = new ArrayList<SysappInfo>();
	SysappAdapter sysappAdapter = null;

	SysappLoader loader = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		getActivity().registerReceiver(receiverInstallResult,
				filterInstallResult);
		getActivity().registerReceiver(receiverUninstallResult,
				filterUninstallResult);
		LogApi.logEnterSysapp();
	}

	@Override
	public void onDestroy() {
		getActivity().unregisterReceiver(receiverInstallResult);
		getActivity().unregisterReceiver(receiverUninstallResult);
		super.onDestroy();
	}

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
		menu.clear();
		MenuItem itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 98,
				R.string.search);
		itemSearch.setIcon(android.R.drawable.ic_menu_search);
		itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		SearchView sv = new SearchView(getActivity());
		sv.setOnQueryTextListener(this);
		itemSearch.setActionView(sv);

		MenuItem itemAdd = menu.add(0, MenuItemIds.MENU_ADD, 99, R.string.add);
		itemAdd.setIcon(android.R.drawable.ic_menu_add);
		itemAdd.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

		MenuItem itemRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 100,
				R.string.refresh);
		itemRefresh.setIcon(android.R.drawable.ic_menu_revert);
		itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_ADD:
			Intent inSelect = new Intent(getActivity(),
					SysappSelectApkActivity.class);
			startActivityForResult(inSelect, RTConsts.REQCODE_SYSAPP_SELECT);
			break;
		case MenuItemIds.MENU_REFRESH:
			doStartLoad();
			break;
		}
		return true;
	}

	protected void doStartLoad() {
		progressSysapp.setAppName(getString(R.string.loading));
		progressSysapp.setVisibility(View.VISIBLE);
		loader.startLoading();
	}

	@Override
	protected int getBarTitle() {
		return R.string.func1_title;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.func1_title_with_path;
	}

	@Override
	public boolean onQueryTextSubmit(String query) {
		return false;
	}

	@Override
	public boolean onQueryTextChange(String newText) {
		sysappAdapter.getFilter().filter(newText);
		return true;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		GlobalInstance.currentSysapp = (SysappInfo) lvSysApp
				.getItemAtPosition(position);
		Intent inApp = new Intent(getActivity(), SysappDetailActivity.class);
		startActivityForResult(inApp, RTConsts.REQCODE_SYSAPP);

	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		Log.e("SysappFragment", "onActivityResult");
		if (resultCode != Activity.RESULT_OK) {
			return;
		}
		switch (requestCode) {
		case RTConsts.REQCODE_SYSAPP:
			boolean needRefresh = data.getBooleanExtra("needRefresh", false);
			if (needRefresh) {

			}
			break;
		}
	}

	@Override
	public void onLoadComplete(Loader<List<SysappInfo>> loader,
			List<SysappInfo> data) {
		Log.e("SysappFragment", "onLoadComplete");
		listSysappAll.clear();
		listSysappAll.addAll(data);
		sysappAdapter.setNewList(listSysappAll);
		progressSysapp.setVisibility(View.GONE);

	}

	@Override
	protected void initComponents() {
		progressSysapp = (DataProgressBar) innerView
				.findViewById(R.id.progressSysapp);
		lvSysApp = (ListView) innerView.findViewById(R.id.lvSysApp);
		lvSysApp.setOnItemClickListener(this);

		sysappAdapter = new SysappAdapter(getActivity().getLayoutInflater(),
				listSysappAll);
		lvSysApp.setAdapter(sysappAdapter);

		progressSysapp.setAppName(getString(R.string.loading));
		progressSysapp.setVisibility(View.VISIBLE);
		loader = new SysappLoader(getActivity());
		loader.registerListener(0, this);
		loader.startLoading();

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_sysapp;
	}

	public class InstallResultReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			if (intent.getIntExtra("result", 0) == 1) {
				doStartLoad();
				Toast.makeText(getActivity(), R.string.install_ok,
						Toast.LENGTH_LONG).show();
			} else {
				Toast.makeText(getActivity(), R.string.install_fail,
						Toast.LENGTH_LONG).show();
			}
		}
	}

	public class UninstallResultReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			if (intent.getIntExtra("result", 0) == 1) {
				doStartLoad();
			} else {
				Toast.makeText(getActivity(), R.string.delete_fail,
						Toast.LENGTH_LONG).show();
			}
		}
	}

	public IntentFilter filterInstallResult = new IntentFilter(
			Actions.ACTION_INSALL_RESULT);
	public InstallResultReceiver receiverInstallResult = new InstallResultReceiver();

	public IntentFilter filterUninstallResult = new IntentFilter(
			Actions.ACTION_UNINSTALL_RESULT);
	public UninstallResultReceiver receiverUninstallResult = new UninstallResultReceiver();

}
