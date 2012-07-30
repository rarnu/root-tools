package com.rarnu.tools.root;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.SysappAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.utils.ApkUtils;

public class SysappMainActivity extends BaseActivity implements
		OnClickListener, OnItemClickListener {

	// [region] field define

	SearchBar sbSysApp;
	DataProgressBar progressSysapp;
	ListView lvSysApp;
	// [/region]

	// [region] variable define
	List<SysappInfo> listSysappAll = new ArrayList<SysappInfo>();
	SysappAdapter sysappAdapter = null;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_sysapp);

		init();
		loadSystemApp();
		setResult(RESULT_OK);
		LogApi.logEnterSysapp();
	}

	// [/region]

	// [region] init

	@Override
	public void init() {
		mappingTitle();
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();
	};

	@Override
	public void mappingComp() {

		sbSysApp = (SearchBar) findViewById(R.id.sbSysapp);
		progressSysapp = (DataProgressBar) findViewById(R.id.progressSysapp);
		lvSysApp = (ListView) findViewById(R.id.lvSysApp);
	}

	@Override
	public void initTitle() {

		tvName.setText(R.string.func1_title);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		btnRight.setText(R.string.refresh);
		btnRight.setVisibility(View.VISIBLE);

		// tbTitle.setText(getString(R.string.func1_title));
		// tbTitle.setRightButtonText(getString(R.string.refresh));
		// tbTitle.getRightButton().setVisibility(View.VISIBLE);
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {
		sbSysApp.setAddButtonVisible(true);
	}

	@Override
	public void initEvents() {
		btnRight.setOnClickListener(this);
		btnLeft.setOnClickListener(this);
		lvSysApp.setOnItemClickListener(this);
		sbSysApp.getAddButton().setOnClickListener(this);
		sbSysApp.getCancelButton().setOnClickListener(this);
		sbSysApp.getEditText().addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count,
					int after) {
			}

			@Override
			public void afterTextChanged(Editable s) {
				if (s == null || sysappAdapter == null) {
					return;
				}
				sysappAdapter.getFilter().filter(sbSysApp.getText().toString());

			}
		});

	}

	// [/region]

	// [region] business logic
	public void loadSystemApp() {
		sbSysApp.setText("");
		progressSysapp.setAppName(getString(R.string.loading));
		progressSysapp.setVisibility(View.VISIBLE);
		btnRight.setEnabled(false);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (listSysappAll != null) {
						sysappAdapter = new SysappAdapter(getLayoutInflater(),
								listSysappAll);
					} else {
						sysappAdapter = null;
					}
					lvSysApp.setAdapter(sysappAdapter);
					progressSysapp.setVisibility(View.GONE);
					btnRight.setEnabled(true);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				listSysappAll = ApkUtils.getSystemApps(SysappMainActivity.this);

				h.sendEmptyMessage(1);
			}
		}).start();
	}

	private void doInstallSystemApp(final String path) {
		progressSysapp.setAppName(getString(R.string.installing));
		progressSysapp.setVisibility(View.VISIBLE);

		LogApi.logInstallSystemApp(path);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					Toast.makeText(
							SysappMainActivity.this,
							(msg.arg1 == 1 ? R.string.install_ok
									: R.string.install_fail), Toast.LENGTH_LONG)
							.show();
					progressSysapp.setVisibility(View.GONE);
					loadSystemApp();
				}
				super.handleMessage(msg);
			}

		};

		ApkUtils.installSystemApp(this, path, h);
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
			loadSystemApp();
			break;
		case R.id.btnCancel:
			sbSysApp.setText("");
			break;
		case R.id.btnAdd:
			Intent inSelect = new Intent(this, SysappSelectApkActivity.class);
			startActivityForResult(inSelect, RTConsts.REQCODE_SYSAPP_SELECT);
			break;
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != RESULT_OK) {
			return;
		}
		switch (requestCode) {
		case RTConsts.REQCODE_SYSAPP:
			boolean needRefresh = data.getBooleanExtra("needRefresh", false);
			if (needRefresh) {
				listSysappAll.remove(GlobalInstance.currentSysapp);
				sysappAdapter.deleteItem(GlobalInstance.currentSysapp);
			}
			break;
		case RTConsts.REQCODE_SYSAPP_SELECT:
			final String apkPath = data.getStringExtra("path");
			File apk = new File(apkPath);
			if (!apk.exists()) {
				return;
			}
			if (!apkPath.equals("")) {
				AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint),
						String.format(
								getResources().getString(R.string.install_apk),
								apk.getName()), getString(R.string.ok),
						new AlertDialogEx.DialogButtonClickListener() {

							@Override
							public void onClick(View v) {
								doInstallSystemApp(apkPath);
							}
						}, getString(R.string.cancel), null);
			}
			break;

		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		GlobalInstance.currentSysapp = (SysappInfo) lvSysApp
				.getItemAtPosition(position);
		Intent inApp = new Intent(this, SysappDetailActivity.class);
		startActivityForResult(inApp, RTConsts.REQCODE_SYSAPP);
	}

	// [/region]

}
