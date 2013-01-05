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
import android.widget.Button;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.TextView;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DataappAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.comp.DataBar;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.fragmentactivity.DataBackupActivity;
import com.rarnu.tools.root.fragmentactivity.DataappReportActivity;
import com.rarnu.tools.root.loader.BackupLoader;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ListUtils;

public class BackupFragment extends BaseFragment implements OnClickListener,
		OnLoadCompleteListener<List<DataappInfo>>, OnQueryTextListener {

	ListView lvData;
	DataBar barData;
	DataProgressBar progressData;

	List<DataappInfo> listDataappAll = new ArrayList<DataappInfo>();
	DataappAdapter dataappAdapter = null;

	BackupLoader loader = null;
	MenuItem itemRefresh;

	private Handler hSelectApp = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showAppSelectedCount();
			}
			super.handleMessage(msg);
		};
	};

	@Override
	protected int getBarTitle() {
		return R.string.func3_title;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.func3_title_with_path;
	}

	@Override
	protected void initComponents() {
		barData = (DataBar) innerView.findViewById(R.id.barData);
		progressData = (DataProgressBar) innerView
				.findViewById(R.id.progressData);
		lvData = (ListView) innerView.findViewById(R.id.lvData);

		dataappAdapter = new DataappAdapter(getActivity().getLayoutInflater(),
				listDataappAll, hSelectApp, 1);
		lvData.setAdapter(dataappAdapter);

		barData.getButton1().setOnClickListener(this);
		barData.getButton2().setOnClickListener(this);
		barData.getCheckBox().setOnClickListener(this);

		loader = new BackupLoader(getActivity());
		loader.registerListener(0, this);
	}

	@Override
	protected void initLogic() {
		doStartLoad();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_backup;
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

	private int getAppSelectedCount() {
		return ListUtils.getListViewSelectedCount(listDataappAll);
	}

	private void showAppSelectedCount() {
		// show appselected count
		try {
			int count = getAppSelectedCount();
			String cap = String.format(
					getResources().getString(R.string.btn_backup), count);
			barData.setButton1Text(cap);
			barData.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
		} catch (Exception e) {

		}
	}

	/*
	private void doBackupT() {
		backuping = true;
		sbData.setText("");
		sbData.getEditText().setEnabled(false);
		lvData.setEnabled(false);
		btnSwitchBackData.setEnabled(false);
		btnSwitchBackData.setBackgroundResource(R.drawable.right_arrow_gray);

		LogApi.logBackupData();
		final int maxCnt = getAppSelectedCount();
		barData.setVisibility(View.GONE);
		progressData.setVisibility(View.VISIBLE);
		ApkUtils.clearOperationLog();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					progressData.setAppName((String) msg.obj);
					progressData.setProgress(String.format("%d / %d", msg.arg1,
							maxCnt));
				}
				if (msg.what == 2) {
					progressData.setVisibility(View.GONE);
					lvData.setEnabled(true);
					sbData.getEditText().setEnabled(true);
					btnSwitchBackData.setEnabled(true);
					btnSwitchBackData
							.setBackgroundResource(R.drawable.right_arrow);
					setListViewItemSelectedStatus(listDataappAll,
							dataappAdapter, hSelectApp, false);
					Intent inReport = new Intent(DataappMainActivity.this,
							DataappReportActivity.class);
					startActivity(inReport);
					backuping = false;
				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				int cnt = 0;
				DataappInfo info;
				for (int i = 0; i < listDataappAll.size(); i++) {
					info = listDataappAll.get(i);
					if (info.checked) {
						cnt++;
						Message msg = new Message();
						msg.what = 1;
						msg.obj = GlobalInstance.pm.getApplicationLabel(
								info.info).toString();
						msg.arg1 = cnt;
						h.sendMessage(msg);

						ApkUtils.backupData(DataappMainActivity.this,
								GlobalInstance.pm
										.getApplicationLabel(info.info)
										.toString(), info.info.sourceDir,
								info.info.packageName, info);

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
			// doBackupT();
			break;
		case R.id.barButton2:
			ListUtils.setListViewItemSelectedStatus(listDataappAll,
					dataappAdapter, hSelectApp, false);
			break;
		case R.id.chkSelAll:
			boolean selected = barData.getCheckBox().isChecked();
			ListUtils.setListViewItemSelectedStatus(listDataappAll,
					dataappAdapter, hSelectApp, selected);
			break;
		}

	}

	private void doStartLoad() {

		barData.setVisibility(View.GONE);
		progressData.setAppName(getString(R.string.loading));
		progressData.setProgress("");
		progressData.setVisibility(View.VISIBLE);
		loader.startLoading();
	}

	@Override
	public void onLoadComplete(Loader<List<DataappInfo>> loader,
			List<DataappInfo> data) {

		listDataappAll.clear();
		if (data != null) {
			listDataappAll.addAll(data);
		}
		dataappAdapter.setNewData(listDataappAll);
		progressData.setVisibility(View.GONE);

		showAppSelectedCount();

	}

	@Override
	public boolean onQueryTextSubmit(String query) {
		return false;
	}

	@Override
	public boolean onQueryTextChange(String newText) {
		if (dataappAdapter != null) {
			dataappAdapter.getFilter().filter(newText);
		}
		return true;
	}

}
