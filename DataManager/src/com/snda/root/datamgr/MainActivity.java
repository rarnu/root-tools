package com.snda.root.datamgr;

import java.util.ArrayList;
import java.util.List;

import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.app.TabActivity;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TabHost;
import android.widget.TabWidget;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.TabHost.OnTabChangeListener;
import android.widget.TabHost.TabSpec;

import com.snda.root.datamgr.adapter.AppAdapter;
import com.snda.root.datamgr.utils.ApkUtils;
import com.snda.root.datamgr.utils.AppInfo;
import com.snda.root.datamgr.utils.Misc;
import com.snda.root.datamgr.utils.RootUtils;
import com.snda.root.datamgr.utils.SDCardUtils;

public class MainActivity extends TabActivity implements OnClickListener,
		OnItemLongClickListener, OnTabChangeListener {
	TabHost tabHost;
	TabSpec tabApp, tabBak;

	ListView lvApplications;
	ProgressBar pbLoadApplication;

	ListView lvData;
	ProgressBar pbLoadData;

	EditText etFilterApp, etFilterData;
	ImageButton btnFilterApp, btnFilterData;

	Button btnBackup, btnClearApp;
	Button btnRestore, btnClearData;

	List<AppInfo> list_app_full = new ArrayList<AppInfo>();
	List<AppInfo> list_app = new ArrayList<AppInfo>();
	AppAdapter adapterApp = null;

	List<AppInfo> list_data_full = new ArrayList<AppInfo>();
	List<AppInfo> list_data = new ArrayList<AppInfo>();
	AppAdapter adapterData = null;

	Handler hSelectApp = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showAppSelectedCount();
			}
			super.handleMessage(msg);
		};
	};

	Handler hSelectData = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showDataSelectedCount();
			}
			super.handleMessage(msg);
		};
	};

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (!SDCardUtils.isSdcardExists()) {
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
					R.string.no_sdcard).setPositiveButton(R.string.ok,
					new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface arg0, int arg1) {
							finish();
						}
					}).show();
			return;
		}

		if (RootUtils.hasRoot() == 0) {
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
					R.string.no_root).setPositiveButton(R.string.ok,
					new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface arg0, int arg1) {
							finish();
						}
					}).show();

			return;
		}

		if (!RootUtils.hasBusybox()) {
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
					R.string.no_busybox).setPositiveButton(R.string.ok,
					new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface arg0, int arg1) {
							finish();
						}
					}).show();

			return;
		}

		SDCardUtils.makeAppDir();

		GlobalInstance.pm = getPackageManager();

		tabHost = getTabHost();

		LayoutInflater.from(this).inflate(R.layout.main,
				tabHost.getTabContentView(), true);
		tabApp = inflateTab(R.id.tab_app, "id_app", getResources().getString(
				R.string.tab_app_title), R.drawable.icon);
		tabBak = inflateTab(R.id.tab_bak, "id_bak", getResources().getString(
				R.string.tab_bak_title), R.drawable.icon);

		tabHost.addTab(tabApp);
		tabHost.addTab(tabBak);

		setTabHost();

		lvApplications = (ListView) findViewById(R.id.lvApplications);
		pbLoadApplication = (ProgressBar) findViewById(R.id.pbLoadApplication);

		btnBackup = (Button) findViewById(R.id.btnBackup);
		btnClearApp = (Button) findViewById(R.id.btnClearApp);

		lvData = (ListView) findViewById(R.id.lvData);
		pbLoadData = (ProgressBar) findViewById(R.id.pbLoadData);
		btnClearData = (Button) findViewById(R.id.btnClearData);
		btnRestore = (Button) findViewById(R.id.btnRestore);

		etFilterApp = (EditText) findViewById(R.id.etFilterApp);
		etFilterData = (EditText) findViewById(R.id.etFilterData);
		btnFilterApp = (ImageButton) findViewById(R.id.btnFilterApp);
		btnFilterData = (ImageButton) findViewById(R.id.btnFilterData);

		btnBackup.setOnClickListener(this);
		btnRestore.setOnClickListener(this);
		btnClearApp.setOnClickListener(this);
		btnClearData.setOnClickListener(this);

		btnFilterApp.setOnClickListener(this);
		btnFilterData.setOnClickListener(this);

		lvData.setOnItemLongClickListener(this);
		tabHost.setOnTabChangedListener(this);

		loadApp();

	}

	private TabSpec inflateTab(int resource, String id, String title, int icon) {
		return tabHost.newTabSpec(id).setIndicator(title, null).setContent(
				resource);
	}

	private void setTabHost() {
		TabWidget tw = tabHost.getTabWidget();

		ViewGroup.LayoutParams vglp = tw.getLayoutParams();
		vglp.height = 48;
		tw.setLayoutParams(vglp);

		for (int i = 0; i < tw.getChildCount(); i++) {
			RelativeLayout rl = (RelativeLayout) tw.getChildTabViewAt(i);
			rl.getLayoutParams().height = 48;
		}
		tw.invalidate();

	}

	private void doFilterT(final String keyword, final int type) {

		switch (type) {
		case 1:
			pbLoadApplication.setVisibility(View.VISIBLE);
			break;
		case 2:
			pbLoadData.setVisibility(View.VISIBLE);
			break;
		}
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					switch (type) {
					case 1:
						lvApplications.setAdapter(adapterApp);
						pbLoadApplication.setVisibility(View.GONE);
						etFilterApp.setText("");
						break;
					case 2:
						lvData.setAdapter(adapterData);
						pbLoadData.setVisibility(View.GONE);
						etFilterData.setText("");
						break;
					}

				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				switch (type) {
				case 1:
					list_app.clear();
					for (AppInfo info : list_app_full) {
						if (GlobalInstance.pm.getApplicationLabel(info.info)
								.toString().contains(keyword)
								|| info.info.packageName.contains(keyword)) {
							list_app.add(info);
						}
					}
					adapterApp = new AppAdapter(getLayoutInflater(), list_app,
							hSelectApp, 1);
					break;
				case 2:
					list_data.clear();
					for (AppInfo info : list_data_full) {
						if (ApkUtils.getLabelFromPackage(MainActivity.this,
								info.info).contains(keyword)
								|| info.info.packageName.contains(keyword)) {
							list_data.add(info);
						}
					}
					adapterData = new AppAdapter(getLayoutInflater(), list_app,
							hSelectData, 2);
					break;
				}
				h.sendEmptyMessage(1);
			}
		}).start();

	}

	public void loadApp() {
		etFilterApp.setText("");
		lvApplications.setVisibility(View.GONE);
		pbLoadApplication.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (list_app != null) {
						adapterApp = new AppAdapter(getLayoutInflater(),
								list_app, hSelectApp, 1);
						lvApplications.setAdapter(adapterApp);
					}
					pbLoadApplication.setVisibility(View.GONE);
					lvApplications.setVisibility(View.VISIBLE);
					showAppSelectedCount();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				list_app_full = ApkUtils.getInstalledApps(MainActivity.this);
				list_app.clear();
				for (AppInfo info : list_app_full) {
					list_app.add(info);
				}
				h.sendEmptyMessage(1);
			}
		}).start();

	}

	public void loadData() {
		etFilterData.setText("");
		lvData.setVisibility(View.GONE);
		pbLoadData.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (list_data != null) {
						adapterData = new AppAdapter(getLayoutInflater(),
								list_data, hSelectData, 2);
						lvData.setAdapter(adapterData);
					}
					pbLoadData.setVisibility(View.GONE);
					lvData.setVisibility(View.VISIBLE);
					showDataSelectedCount();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				list_data_full = ApkUtils.getBackupedApps(MainActivity.this);
				list_data.clear();
				for (AppInfo info : list_data_full) {
					list_data.add(info);
				}
				h.sendEmptyMessage(1);
			}
		}).start();

	}

	private int getAppSelectedCount() {
		return getListViewSelectedCount(lvApplications);
	}

	private int getDataSelectedCount() {
		return getListViewSelectedCount(lvData);
	}

	private int getListViewSelectedCount(ListView lv) {
		int count = 0;

		for (int i = 0; i < lv.getCount(); i++) {
			if (((AppInfo) lv.getItemAtPosition(i)).checked) {
				count++;
			}
		}
		return count;
	}

	private void showAppSelectedCount() {
		int count = getAppSelectedCount();
		String cap = String.format(getResources()
				.getString(R.string.btn_backup), count);
		btnBackup.setText(cap);
		btnBackup.setEnabled(count != 0);
		btnClearApp.setEnabled(count != 0);
		etFilterApp.setEnabled(list_app.size() != 0);
	}

	private void showDataSelectedCount() {
		int count = getDataSelectedCount();
		String cap = String.format(getResources().getString(
				R.string.btn_restore), count);
		btnRestore.setText(cap);
		btnRestore.setEnabled(count != 0);
		btnClearData.setEnabled(count != 0);
		etFilterData.setEnabled(list_data.size() != 0);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnBackup:
			doBackupT();
			break;
		case R.id.btnRestore:
			doRestoreT();
			break;
		case R.id.btnClearApp:
			setListViewItemUnselected(lvApplications, hSelectApp);
			break;
		case R.id.btnClearData:
			setListViewItemUnselected(lvData, hSelectData);
			break;
		case R.id.btnFilterApp:
			doFilterT(etFilterApp.getText().toString(), 1);
			break;
		case R.id.btnFilterData:
			doFilterT(etFilterData.getText().toString(), 2);
			break;
		}
	}

	private void setListViewItemUnselected(ListView lv, Handler h) {
		for (int i = 0; i < lv.getCount(); i++) {

			((AppInfo) lv.getItemAtPosition(i)).checked = false;

			RelativeLayout viewItem = (RelativeLayout) lv.getChildAt(i);
			if (viewItem != null) {
				for (int j = 0; j < viewItem.getChildCount(); j++) {
					if (viewItem.getChildAt(j) instanceof CheckBox) {
						((CheckBox) viewItem.getChildAt(j)).setChecked(false);
					}
				}
			}
		}
		h.sendEmptyMessage(1);
	}

	private void doBackupT() {

		GlobalInstance.pref_override = Misc.getConfig(this, "pref_override",
				true);
		GlobalInstance.pref_apk = Misc.getConfig(this, "pref_apk", true);

		ApkUtils.clearOperationLog();
		final ProgressDialog pDlg = new ProgressDialog(this);
		pDlg.setCancelable(false);
		pDlg.setProgressStyle(ProgressDialog.STYLE_SPINNER);
		pDlg.show();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					pDlg.setMessage(getResources()
							.getString(R.string.backuping)
							+ (String) msg.obj);
				}
				if (msg.what == 2) {
					pDlg.dismiss();
					setListViewItemUnselected(lvApplications, hSelectApp);
					Intent inReport = new Intent(MainActivity.this,
							ReportActivity.class);
					startActivity(inReport);
				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				int cnt = 0;
				AppInfo info;
				for (int i = 0; i < lvApplications.getCount(); i++) {
					info = (AppInfo) lvApplications.getItemAtPosition(i);
					if (info.checked) {
						Message msg = new Message();
						msg.what = 1;
						msg.obj = GlobalInstance.pm.getApplicationLabel(
								info.info).toString();
						h.sendMessage(msg);

						ApkUtils.backupData(MainActivity.this,
								GlobalInstance.pm
										.getApplicationLabel(info.info)
										.toString(), info.info.sourceDir,
								info.info.packageName, info);
						cnt++;
					}

				}
				h.sendEmptyMessage(2);

			}
		}).start();

	}

	private void doRestoreT() {

		GlobalInstance.pref_override = Misc.getConfig(this, "pref_override",
				true);
		GlobalInstance.pref_apk = Misc.getConfig(this, "pref_apk", true);

		ApkUtils.clearOperationLog();
		final ProgressDialog pDlg = new ProgressDialog(this);
		pDlg.setCancelable(false);
		pDlg.setProgressStyle(ProgressDialog.STYLE_SPINNER);
		pDlg.show();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					pDlg.setMessage(getResources()
							.getString(R.string.restoring)
							+ (String) msg.obj);
				}
				if (msg.what == 2) {
					pDlg.dismiss();
					setListViewItemUnselected(lvData, hSelectData);
					Intent inReport = new Intent(MainActivity.this,
							ReportActivity.class);
					startActivity(inReport);
				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				int cnt = 0;
				AppInfo info;
				for (int i = 0; i < lvData.getCount(); i++) {
					info = (AppInfo) lvData.getItemAtPosition(i);
					if (info.checked) {
						Message msg = new Message();
						msg.what = 1;
						msg.obj = ApkUtils.getLabelFromPackage(
								MainActivity.this, info.info);

						h.sendMessage(msg);

						ApkUtils
								.restoreData(MainActivity.this, ApkUtils
										.getLabelFromPackage(MainActivity.this,
												info.info),
										info.info.packageName, info);
						cnt++;
					}

				}
				h.sendEmptyMessage(2);

			}
		}).start();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		getMenuInflater().inflate(R.menu.menu_settings, menu);
		return super.onCreateOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.mi_settings:
			Intent inSettings = new Intent(this, SettingsActivity.class);
			startActivity(inSettings);
			break;
		case R.id.mi_about:
			Intent inHelp = new Intent(this, HelpActivity.class);
			startActivity(inHelp);
			break;
		}
		return super.onOptionsItemSelected(item);
	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view,
			final int position, long id) {
		final AppInfo item = (AppInfo) lvData.getItemAtPosition(position);
		new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
				R.string.confirm_delete).setPositiveButton(R.string.ok,
				new DialogInterface.OnClickListener() {

					@Override
					public void onClick(DialogInterface dialog, int which) {
						ApkUtils.deleteBackupData(item.info.packageName);
						int p = lvData.getFirstVisiblePosition();
						list_data.remove(position);
						adapterData = new AppAdapter(getLayoutInflater(),
								list_data, hSelectData, 2);
						lvData.setAdapter(adapterData);
						lvData.setSelectionFromTop(p, -1);
					}
				}).setNegativeButton(R.string.cancel, null).show();
		return false;
	}

	@Override
	public void onTabChanged(String tabId) {
		if (tabId.equals("id_app")) {
			loadApp();
		} else if (tabId.equals("id_bak")) {
			loadData();
		}

	}

}