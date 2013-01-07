package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
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
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DataappAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataBar;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.loader.RestoreLoader;
import com.rarnu.tools.root.service.DataRestoreService;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ListUtils;

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
	public void onResume() {
		super.onResume();
		getActivity().registerReceiver(receiver, filter);
		getActivity().registerReceiver(progressReceiver, progressFilter);
		getActivity().registerReceiver(backupReceiver, backupFilter);
		setRestoreState(false);
	}
	
	public void onPause() {
		getActivity().unregisterReceiver(receiver);
		getActivity().unregisterReceiver(progressReceiver);
		getActivity().unregisterReceiver(backupReceiver);
		super.onPause();
	};

	@Override
	protected void initComponents() {
		barBackData = (DataBar) innerView.findViewById(R.id.barBackData);
		progressBackData = (DataProgressBar) innerView
				.findViewById(R.id.progressBackData);
		lvBackData = (ListView) innerView.findViewById(R.id.lvBackData);

		backDataappAdapter = new DataappAdapter(getActivity()
				.getLayoutInflater(), listBackDataappAll, hSelectData, 2);
		lvBackData.setAdapter(backDataappAdapter);

		barBackData.setCheckBoxVisible(true);
		
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
		AlertDialogEx.showAlertDialogEx(getActivity(),
				getString(R.string.hint),
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

	private void setRestoreState(boolean resotre) {
		backDataappAdapter.setAdapterCheckable(!resotre);
		if (resotre) {
			barBackData.setVisibility(View.GONE);
			progressBackData.setVisibility(View.VISIBLE);

		} else {
			progressBackData.setVisibility(View.GONE);
			ListUtils.setListViewItemSelectedStatus(listBackDataappAll,
					backDataappAdapter, hSelectData, false);
		}
	}

	private void setOtherProcState(boolean backup) {
		backDataappAdapter.setAdapterCheckable(!backup);
		if (backup) {
			barBackData.setVisibility(View.GONE);
			progressBackData.setVisibility(View.VISIBLE);
			progressBackData.setAppName(getString(R.string.backuping));
		} else {
			progressBackData.setVisibility(View.GONE);
		}
	}

	private void setRestoreProgress(String name, int position, int total) {
		progressBackData.setAppName(getString(R.string.restoring) + name);
		progressBackData.setProgress(String.format("%d / %d", position, total));
	}

	private void doRestore() {
		LogApi.logRestoreData();
		setRestoreState(true);
		List<DataappInfo> listOperate = new ArrayList<DataappInfo>();
		for (int i = 0; i < listBackDataappAll.size(); i++) {
			if (listBackDataappAll.get(i).checked) {
				listOperate.add(listBackDataappAll.get(i));
			}
		}
		ListUtils.setOperateList(listOperate);
		progressBackData.setAppName(getString(R.string.restoring));
		Intent inRestoreService = new Intent(getActivity(),
				DataRestoreService.class);
		inRestoreService.putExtra("command", "restore");
		inRestoreService.putExtra("id", RTConsts.NOTIFY_ID_RESTORE);
		inRestoreService.putExtra("title", R.string.func3p_title);
		inRestoreService.putExtra("desc", R.string.restore_ok);
		getActivity().startService(inRestoreService);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.barButton1:
			doRestore();
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

	public class RestoreReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			boolean operating = intent.getBooleanExtra("operating", false);
			if (!operating) {
				Intent inRestoreService = new Intent(getActivity(),
						DataRestoreService.class);
				getActivity().stopService(inRestoreService);
			}
			setRestoreState(operating);
		}
	}

	public RestoreReceiver receiver = new RestoreReceiver();
	public IntentFilter filter = new IntentFilter(Actions.ACTION_RESTORE);

	public class RestoreProgressReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			int size = intent.getIntExtra("size", 0);
			int position = intent.getIntExtra("position", 0);
			String name = intent.getStringExtra("name");
			setRestoreProgress(name, position, size);
		}

	}

	public RestoreProgressReceiver progressReceiver = new RestoreProgressReceiver();
	public IntentFilter progressFilter = new IntentFilter(
			Actions.ACTION_RESTORE_PROGRESS);

	public class BackupReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			boolean operating = intent.getBooleanExtra("operating", false);
			setOtherProcState(operating);
		}
	}

	public BackupReceiver backupReceiver = new BackupReceiver();
	public IntentFilter backupFilter = new IntentFilter(Actions.ACTION_BACKUP);
}
