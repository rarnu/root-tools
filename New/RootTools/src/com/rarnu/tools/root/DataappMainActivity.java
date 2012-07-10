package com.rarnu.tools.root;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.tools.root.adapter.DataappAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.ActivityIntf;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataBar;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.comp.TitleBar;
import com.rarnu.tools.root.utils.ApkUtils;

public class DataappMainActivity extends Activity implements ActivityIntf, OnClickListener, OnItemLongClickListener {

	// [region] field define
	TitleBar tbTitle;
	RelativeLayout pageData, pageBackData;
	ListView lvData, lvBackData;
	SearchBar sbData, sbBackData;
	DataBar barData, barBackData;
	DataProgressBar progressData, progressBackData;
	Button btnSwitchBackData, btnSwitchData;
	TextView tvCurrentFunc;
	// [/region]

	// [region] variable define
	int currentDataPage = 1;
	boolean backuping = false, restoring = false, deleting = false;
	boolean dataLoading = false, backdataLoading = false;

	List<DataappInfo> listDataappAll = new ArrayList<DataappInfo>();
	List<DataappInfo> listBackDataappAll = new ArrayList<DataappInfo>();
	DataappAdapter dataappAdapter = null;
	DataappAdapter backDataappAdapter = null;
	// [/region]

	// [region] handler define
	private Handler hSelectApp = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showAppSelectedCount();
			}
			super.handleMessage(msg);
		};
	};

	private Handler hSelectData = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showDataSelectedCount();
			}
			super.handleMessage(msg);
		};
	};

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_data);
		init();
		switchDataPage();
		LogApi.logEnterData();
	}

	// [/region]

	// [region] init

	@Override
	public void init() {
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();
	};

	@Override
	public void mappingComp() {
		tbTitle = (TitleBar) findViewById(R.id.tbTitle);
		pageData = (RelativeLayout) findViewById(R.id.pageData);
		pageBackData = (RelativeLayout) findViewById(R.id.pageBackData);
		barData = (DataBar) findViewById(R.id.barData);
		barBackData = (DataBar) findViewById(R.id.barBackData);
		progressData = (DataProgressBar) findViewById(R.id.progressData);
		progressBackData = (DataProgressBar) findViewById(R.id.progressBackData);
		sbData = (SearchBar) findViewById(R.id.sbData);
		sbBackData = (SearchBar) findViewById(R.id.sbBackData);
		lvData = (ListView) findViewById(R.id.lvData);
		lvBackData = (ListView) findViewById(R.id.lvBackData);
		btnSwitchData = (Button) findViewById(R.id.btnSwitchData);
		btnSwitchBackData = (Button) findViewById(R.id.btnSwitchBackData);
		tvCurrentFunc = (TextView) findViewById(R.id.tvCurrentFunc);
	}

	@Override
	public void initTitle() {
		tbTitle.setLeftButtonText(getString(R.string.back));
		tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		tbTitle.setRightButtonText(getString(R.string.refresh));
		tbTitle.getRightButton().setVisibility(View.VISIBLE);
		tbTitle.setText(getString(R.string.func2_title));
	}

	@Override
	public void initSearchBar() {
		sbData.setAddButtonVisible(false);
		sbBackData.setAddButtonVisible(false);
		barData.setCheckBoxVisible(true);
		barBackData.setCheckBoxVisible(true);
	}

	@Override
	public void initEvents() {
		sbData.getCancelButton().setOnClickListener(this);
		sbData.getEditText().addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {
			}

			@Override
			public void afterTextChanged(Editable s) {
				if (s == null || dataappAdapter == null) {
					return;
				}
				dataappAdapter.getFilter().filter(sbData.getText().toString());
			}
		});

		sbBackData.getCancelButton().setOnClickListener(this);
		sbBackData.getEditText().addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {

			}

			@Override
			public void afterTextChanged(Editable s) {
				if (s == null || backDataappAdapter == null) {
					return;
				}
				backDataappAdapter.getFilter().filter(sbBackData.getText().toString());
			}
		});

		lvBackData.setOnItemLongClickListener(this);
		btnSwitchBackData.setOnClickListener(this);
		btnSwitchData.setOnClickListener(this);
		barData.getButton1().setOnClickListener(this);
		barBackData.getButton1().setOnClickListener(this);
		barData.getButton2().setOnClickListener(this);
		barBackData.getButton2().setOnClickListener(this);
		barData.getCheckBox().setOnClickListener(this);
		barBackData.getCheckBox().setOnClickListener(this);
		tbTitle.getLeftButton().setOnClickListener(this);
		tbTitle.getRightButton().setOnClickListener(this);

	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.btnRight:
			if (currentDataPage == 1) {
				if ((!dataLoading) && (!backuping)) {
					sbData.setText("");
					loadDataApp();
				}
			} else {
				if ((!backdataLoading) && (!restoring) && (!deleting)) {
					sbBackData.setText("");
					loadBackDataApp();
				}
			}
			break;
		case R.id.btnCancel:
			if (currentDataPage == 1) {
				sbData.setText("");
			} else {
				sbBackData.setText("");
			}
			break;
		case R.id.btnSwitchData:
			currentDataPage = 1;
			switchDataPage();
			break;
		case R.id.btnSwitchBackData:
			currentDataPage = 2;
			switchDataPage();
			break;

		case R.id.barButton1:

			if (currentDataPage == 1) {
				doBackupT();
			} else {
				doRestoreT();
			}

			break;
		case R.id.barButton2:

			if (currentDataPage == 1) {
				setListViewItemSelectedStatus(listDataappAll, dataappAdapter, hSelectApp, false);
			} else {
				setListViewItemSelectedStatus(listBackDataappAll, backDataappAdapter, hSelectData, false);
			}

			break;
		case R.id.chkSelAll:
			if (currentDataPage == 1) {
				boolean selected = barData.getCheckBox().isChecked();
				setListViewItemSelectedStatus(listDataappAll, dataappAdapter, hSelectApp, selected);

			} else {
				boolean selected = barBackData.getCheckBox().isChecked();
				setListViewItemSelectedStatus(listBackDataappAll, backDataappAdapter, hSelectData, selected);
			}
			break;
		}
	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
		DataappInfo item = ((DataappInfo) lvBackData.getItemAtPosition(position));
		confirmDeleteBackup(item);
		return false;
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			if (backuping || restoring) {
				return true;
			}
		}
		return super.onKeyDown(keyCode, event);
	}

	// [/region]

	// [region] bussiness logic

	private int getAppSelectedCount() {
		return getListViewSelectedCount(listDataappAll);
	}

	private int getDataSelectedCount() {
		return getListViewSelectedCount(listBackDataappAll);
	}

	private int getListViewSelectedCount(List<DataappInfo> list) {
		int count = 0;

		for (int i = 0; i < list.size(); i++) {
			if (list.get(i).checked) {
				count++;
			}
		}
		return count;
	}

	private void showAppSelectedCount() {
		// show appselected count
		int count = getAppSelectedCount();
		String cap = String.format(getResources().getString(R.string.btn_backup), count);
		barData.setButton1Text(cap);
		barData.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
	}

	private void showDataSelectedCount() {
		int count = getDataSelectedCount();
		String cap = String.format(getResources().getString(R.string.btn_restore), count);
		barBackData.setButton1Text(cap);
		barBackData.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
	}

	private void switchDataPage() {
		tvCurrentFunc.setText(currentDataPage == 1 ? R.string.backup_data : R.string.restore_data);
		btnSwitchData.setEnabled(currentDataPage == 1 ? false : true);
		btnSwitchBackData.setEnabled(currentDataPage == 1 ? true : false);
		btnSwitchData.setBackgroundResource(currentDataPage == 1 ? R.drawable.left_arrow_gray : R.drawable.left_arrow);
		btnSwitchBackData.setBackgroundResource(currentDataPage == 1 ? R.drawable.right_arrow
				: R.drawable.right_arrow_gray);
		pageData.setVisibility(currentDataPage == 1 ? View.VISIBLE : View.GONE);
		pageBackData.setVisibility(currentDataPage == 1 ? View.GONE : View.VISIBLE);

		if (currentDataPage == 1) {
			if (!backuping) {
				loadDataApp();
			}
		} else {
			if (!restoring) {
				loadBackDataApp();
			}
		}
	}

	public void loadDataApp() {
		sbData.setText("");
		barData.setVisibility(View.GONE);
		progressData.setAppName(getString(R.string.loading));
		progressData.setProgress("");
		progressData.setVisibility(View.VISIBLE);
		dataLoading = true;

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (listDataappAll != null) {
						dataappAdapter = new DataappAdapter(getLayoutInflater(), listDataappAll, hSelectApp, 1);

					} else {
						dataappAdapter = null;
					}
					progressData.setVisibility(View.GONE);
					lvData.setAdapter(dataappAdapter);
					showAppSelectedCount();

					dataLoading = false;
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				listDataappAll = ApkUtils.getInstalledApps(DataappMainActivity.this, false);

				h.sendEmptyMessage(1);
			}
		}).start();

	}

	public void loadBackDataApp() {
		sbBackData.setText("");
		barBackData.setVisibility(View.GONE);
		progressBackData.setAppName(getString(R.string.loading));
		progressBackData.setProgress("");
		progressBackData.setVisibility(View.VISIBLE);

		backdataLoading = true;

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (listBackDataappAll != null) {
						backDataappAdapter = new DataappAdapter(getLayoutInflater(), listBackDataappAll, hSelectData, 2);

					} else {
						backDataappAdapter = null;
					}
					progressBackData.setVisibility(View.GONE);
					lvBackData.setAdapter(backDataappAdapter);
					showDataSelectedCount();
					backdataLoading = false;
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				listBackDataappAll = ApkUtils.getBackupedApps(DataappMainActivity.this);

				h.sendEmptyMessage(1);
			}
		}).start();

	}

	private void setListViewItemSelectedStatus(List<DataappInfo> list, BaseAdapter adapter, Handler h, boolean selected) {
		for (int i = 0; i < list.size(); i++) {
			list.get(i).checked = selected;
		}
		adapter.notifyDataSetChanged();
		h.sendEmptyMessage(1);
	}

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
					progressData.setProgress(String.format("%d / %d", msg.arg1, maxCnt));
				}
				if (msg.what == 2) {
					progressData.setVisibility(View.GONE);
					lvData.setEnabled(true);
					sbData.getEditText().setEnabled(true);
					btnSwitchBackData.setEnabled(true);
					btnSwitchBackData.setBackgroundResource(R.drawable.right_arrow);
					setListViewItemSelectedStatus(listDataappAll, dataappAdapter, hSelectApp, false);
					Intent inReport = new Intent(DataappMainActivity.this, DataappReportActivity.class);
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
						msg.obj = GlobalInstance.pm.getApplicationLabel(info.info).toString();
						msg.arg1 = cnt;
						h.sendMessage(msg);

						ApkUtils.backupData(DataappMainActivity.this, GlobalInstance.pm.getApplicationLabel(info.info)
								.toString(), info.info.sourceDir, info.info.packageName, info);

					}

				}
				h.sendEmptyMessage(2);

			}
		}).start();

	}

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
					progressBackData.setProgress(String.format("%d / %d", msg.arg1, maxCnt));
				}
				if (msg.what == 2) {
					progressBackData.setVisibility(View.GONE);
					lvBackData.setEnabled(true);
					sbBackData.getEditText().setEnabled(true);
					btnSwitchData.setEnabled(true);
					btnSwitchData.setBackgroundResource(R.drawable.left_arrow);
					setListViewItemSelectedStatus(listBackDataappAll, backDataappAdapter, hSelectData, false);
					Intent inReport = new Intent(DataappMainActivity.this, DataappReportActivity.class);
					startActivity(inReport);
					restoring = false;
				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				int cnt = 0;
				DataappInfo info;
				for (int i = 0; i < listBackDataappAll.size(); i++) {
					info = listBackDataappAll.get(i);
					if (info.checked) {
						cnt++;
						Message msg = new Message();
						msg.what = 1;
						msg.obj = ApkUtils.getLabelFromPackage(DataappMainActivity.this, info.info);
						msg.arg1 = cnt;
						h.sendMessage(msg);

						ApkUtils.restoreData(DataappMainActivity.this,
								ApkUtils.getLabelFromPackage(DataappMainActivity.this, info.info),
								info.info.packageName, info);
					}

				}
				h.sendEmptyMessage(2);

			}
		}).start();
	}

	private void confirmDeleteBackup(final DataappInfo item) {
		AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint), getString(R.string.confirm_delete_backup),
				getString(R.string.ok), new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						doDeleteBackupT(item);
					}
				}, getString(R.string.cancel), null);
	}

	private void doDeleteBackupT(final DataappInfo item) {
		deleting = true;
		lvBackData.setEnabled(false);
		btnSwitchData.setEnabled(false);
		btnSwitchData.setBackgroundResource(R.drawable.left_arrow_gray);

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
					btnSwitchData.setEnabled(true);
					btnSwitchData.setBackgroundResource(R.drawable.left_arrow);
					deleting = false;
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

	// [/region]
}
