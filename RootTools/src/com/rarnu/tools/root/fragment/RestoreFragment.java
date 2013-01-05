package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.TextView;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DataappAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataBar;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.fragmentactivity.DataBackupActivity;
import com.rarnu.tools.root.fragmentactivity.DataappReportActivity;
import com.rarnu.tools.root.loader.RestoreLoader;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ListUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class RestoreFragment extends BaseFragment implements
		OnItemLongClickListener, OnClickListener,
		OnLoadCompleteListener<List<DataappInfo>>, OnQueryTextListener {

	ListView lvBackData;
	DataBar barBackData;
	DataProgressBar progressBackData;

	List<DataappInfo> listBackDataappAll = new ArrayList<DataappInfo>();
	DataappAdapter backDataappAdapter = null;

	RestoreLoader loader = null;
	MenuItem itemRefresh;

	private Handler hSelectData = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showDataSelectedCount();
			}
			super.handleMessage(msg);
		};
	};

	@Override
	protected int getBarTitle() {
		return R.string.func3p_title;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.func3p_title_with_path;
	}

	@Override
	protected void initComponents() {
		barBackData = (DataBar) innerView.findViewById(R.id.barBackData);
		progressBackData = (DataProgressBar) innerView
				.findViewById(R.id.progressBackData);
		lvBackData = (ListView) innerView.findViewById(R.id.lvBackData);

		backDataappAdapter = new DataappAdapter(getActivity()
				.getLayoutInflater(), listBackDataappAll, hSelectData, 2);
		lvBackData.setAdapter(backDataappAdapter);

		lvBackData.setOnItemLongClickListener(this);
		barBackData.getButton1().setOnClickListener(this);
		barBackData.getButton2().setOnClickListener(this);
		barBackData.getCheckBox().setOnClickListener(this);

		loader = new RestoreLoader(getActivity());
		loader.registerListener(0, this);
	}

	@Override
	protected void initLogic() {
		doStartLoad();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_restore;
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

		itemRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 99,
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

	private int getDataSelectedCount() {
		return ListUtils.getListViewSelectedCount(listBackDataappAll);
	}

	private void showDataSelectedCount() {
		try {
			int count = getDataSelectedCount();
			String cap = String.format(
					getResources().getString(R.string.btn_restore), count);
			barBackData.setButton1Text(cap);
			barBackData.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
		} catch (Exception e) {

		}
	}

	private void confirmDeleteBackup(final DataappInfo item) {
		AlertDialogEx.showAlertDialogEx(getActivity(), getString(R.string.hint),
				getString(R.string.confirm_delete_backup),
				getString(R.string.ok),
				new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						doDeleteBackupT(item);
					}
				}, getString(R.string.cancel), null);
	}

	private void doDeleteBackupT(final DataappInfo item) {
		lvBackData.setEnabled(false);
		progressBackData.setAppName(getString(R.string.deleting));
		progressBackData.setVisibility(View.VISIBLE);

		LogApi.logDeleteData();

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					listBackDataappAll.remove(item);
					backDataappAdapter.deleteItem(item);

					progressBackData.setVisibility(View.GONE);
					lvBackData.setEnabled(true);
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				ApkUtils.deleteBackupData(item.info.packageName);
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	/*
	private void doRestoreT() {

		restoring = true;
		sbBackData.setText("");
		sbBackData.getEditText().setEnabled(false);
		lvBackData.setEnabled(false);
		btnSwitchData.setEnabled(false);
		btnSwitchData.setBackgroundResource(R.drawable.left_arrow_gray);

		LogApi.logRestoreData();
		final int maxCnt = getDataSelectedCount();
		barBackData.setVisibility(View.GONE);
		progressBackData.setVisibility(View.VISIBLE);

		ApkUtils.clearOperationLog();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					progressBackData.setAppName((String) msg.obj);
					progressBackData.setProgress(String.format("%d / %d",
							msg.arg1, maxCnt));
				}
				if (msg.what == 2) {
					progressBackData.setVisibility(View.GONE);
					lvBackData.setEnabled(true);
					sbBackData.getEditText().setEnabled(true);
					btnSwitchData.setEnabled(true);
					btnSwitchData.setBackgroundResource(R.drawable.left_arrow);
					setListViewItemSelectedStatus(listBackDataappAll,
							backDataappAdapter, hSelectData, false);
					Intent inReport = new Intent(DataappMainActivity.this,
							DataappReportActivity.class);
					startActivity(inReport);
					restoring = false;
				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				RootUtils.runCommand("pm set-install-location 1", true);

				int cnt = 0;
				DataappInfo info;
				for (int i = 0; i < listBackDataappAll.size(); i++) {
					info = listBackDataappAll.get(i);
					if (info.checked) {
						cnt++;
						Message msg = new Message();
						msg.what = 1;
						msg.obj = ApkUtils.getLabelFromPackage(
								DataappMainActivity.this, info.info);
						msg.arg1 = cnt;
						h.sendMessage(msg);

						ApkUtils.restoreData(DataappMainActivity.this, ApkUtils
								.getLabelFromPackage(DataappMainActivity.this,
										info.info), info.info.packageName, info);
					}

				}
				h.sendEmptyMessage(2);

			}
		}).start();
	}
	*/

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.barButton1:
			// doRestoreT();
			break;
		case R.id.barButton2:
			ListUtils.setListViewItemSelectedStatus(listBackDataappAll,
					backDataappAdapter, hSelectData, false);
			break;
		case R.id.chkSelAll:
			boolean selected = barBackData.getCheckBox().isChecked();
			ListUtils.setListViewItemSelectedStatus(listBackDataappAll,
					backDataappAdapter, hSelectData, selected);
			break;
		}

	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view,
			int position, long id) {
		DataappInfo item = ((DataappInfo) lvBackData
				.getItemAtPosition(position));
		confirmDeleteBackup(item);
		return false;
	}

	private void doStartLoad() {

		barBackData.setVisibility(View.GONE);
		progressBackData.setAppName(getString(R.string.loading));
		progressBackData.setProgress("");
		progressBackData.setVisibility(View.VISIBLE);

		loader.startLoading();
	}

	@Override
	public void onLoadComplete(Loader<List<DataappInfo>> loader,
			List<DataappInfo> data) {

		listBackDataappAll.clear();
		if (data != null) {
			listBackDataappAll.addAll(data);
		}

		backDataappAdapter.setNewData(listBackDataappAll);
		progressBackData.setVisibility(View.GONE);
		showDataSelectedCount();

	}

	@Override
	public boolean onQueryTextSubmit(String query) {
		return false;
	}

	@Override
	public boolean onQueryTextChange(String newText) {
		if (backDataappAdapter != null) {
			backDataappAdapter.getFilter().filter(newText);
		}
		return false;
	}
}
