package com.rarnu.tools.root;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.EnableappAdapter;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ComponentUtils;

public class EnableappMainActivity extends BaseActivity implements
		OnClickListener, OnItemLongClickListener {

	// [region] field define
	SearchBar sbEnableapp;
	ListView lvEnableApp;
	DataProgressBar progressEnableapp;
	TextView tvOperateHint;
	// [/region]

	// [region] variable define
	boolean enableappLoading = false;
	EnableappAdapter enableappAdapter;
	List<EnableappInfo> listEnableappAll;

	// [/region]
	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_enableapp);

		loadEnableApp();
	}

	// [/region]

	// [region] init
	

	
	public void mappingComp() {
		sbEnableapp = (SearchBar) findViewById(R.id.sbEnableapp);
		lvEnableApp = (ListView) findViewById(R.id.lvEnableApp);
		progressEnableapp = (DataProgressBar) findViewById(R.id.progressEnableapp);
		tvOperateHint = (TextView) findViewById(R.id.tvOperateHint);
	}

	


	
	public void initEvents() {
		

		lvEnableApp.setOnItemLongClickListener(this);
		sbEnableapp.getAddButton().setOnClickListener(this);
		sbEnableapp.getCancelButton().setOnClickListener(this);
		sbEnableapp.getEditText().addTextChangedListener(new TextWatcher() {

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
				if (s == null || enableappAdapter == null) {
					return;
				}
				enableappAdapter.getFilter().filter(
						sbEnableapp.getText().toString());

			}
		});

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
			loadEnableApp();
			break;
		case R.id.btnCancel:
			sbEnableapp.setText("");
			break;
		}

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
				Toast.makeText(this, R.string.change_package_status_fail,
						Toast.LENGTH_LONG).show();
			}
		} else if (info.type == 1) {

			AlertDialogEx.showAlertDialogEx(this,
					getString(R.string.func2_title),
					getString(R.string.data_app_uninstall),
					getString(R.string.ok),
					new AlertDialogEx.DialogButtonClickListener() {

						@Override
						public void onClick(View v) {
							if (ApkUtils.uninstallApk(info.info.packageName)) {
								enableappAdapter.deleteItem(info);
							} else {
								Toast.makeText(EnableappMainActivity.this,
										R.string.cannot_uninstall_package,
										Toast.LENGTH_LONG).show();
							}
						}
					}, getString(R.string.cancel), null);

		} else {
			Toast.makeText(this, R.string.cannot_change_package_status,
					Toast.LENGTH_LONG).show();
		}

		return true;

	}

	// [/region]

	// [region] business logic

	public void loadEnableApp() {
		tvOperateHint.setVisibility(View.GONE);
		sbEnableapp.setText("");
		progressEnableapp.setAppName(getString(R.string.loading));
		progressEnableapp.setProgress("");
		progressEnableapp.setVisibility(View.VISIBLE);
		enableappLoading = true;

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (listEnableappAll != null) {
						enableappAdapter = new EnableappAdapter(
								getLayoutInflater(), listEnableappAll);
						enableappAdapter.sort();
					} else {
						enableappAdapter = null;
					}
					progressEnableapp.setVisibility(View.GONE);
					tvOperateHint.setVisibility(View.VISIBLE);
					lvEnableApp.setAdapter(enableappAdapter);

					enableappLoading = false;
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				listEnableappAll = ApkUtils
						.getEnabledApplications(EnableappMainActivity.this);

				h.sendEmptyMessage(1);
			}
		}).start();

	}
	// [/region]

	@Override
	public Fragment replaceFragment() {
		// TODO Auto-generated method stub
		return null;
	}

}
