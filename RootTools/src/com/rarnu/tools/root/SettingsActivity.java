package com.rarnu.tools.root;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.CheckBox;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.LineEditText;
import com.rarnu.tools.root.utils.ApkUtils;

public class SettingsActivity extends BaseActivity implements OnClickListener {

	// [region] field define

	RelativeLayout layAllowDeleteLevel0, layAlsoDeleteData,
			layBackupBeforeDelete, layOverrideBackuped, layReinstallApk,
			layKillProcessBeforeClean, layKillIgnoreList, layNameServer,
			layManualEditHosts, layCleanDeprecated, layDeleteAllBackupData;

	CheckBox imgAllowDeleteLevel0, imgAlsoDeleteData, imgBackupBeforeDelete,
			imgOverrideBackuped, imgReinstallApk, imgKillProcessBeforeClean;

	LineEditText etNameServer;

	// [/region]

	// [region] life circle
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_settings);
		init();
		LogApi.logEnterSystemSettings();
	}

	// [/region]

	// [region] event

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.imgAllowDeleteLevel0:
			GlobalInstance.allowDeleteLevel0 = !GlobalInstance.allowDeleteLevel0;
			RTConfig.setAllowDeleteLevel0(this,
					GlobalInstance.allowDeleteLevel0);
			initConfigValues();
			break;
		case R.id.imgAlsoDeleteData:
			GlobalInstance.alsoDeleteData = !GlobalInstance.alsoDeleteData;
			RTConfig.setAlsoDeleteData(this, GlobalInstance.alsoDeleteData);
			initConfigValues();
			break;
		case R.id.imgBackupBeforeDelete:
			GlobalInstance.backupBeforeDelete = !GlobalInstance.backupBeforeDelete;
			RTConfig.setBackupBeforeDelete(this,
					GlobalInstance.backupBeforeDelete);
			initConfigValues();
			break;
		case R.id.imgOverrideBackuped:
			GlobalInstance.overrideBackuped = !GlobalInstance.overrideBackuped;
			RTConfig.setOverrideBackuped(this, GlobalInstance.overrideBackuped);
			initConfigValues();
			break;
		case R.id.imgReinstallApk:
			GlobalInstance.reinstallApk = !GlobalInstance.reinstallApk;
			RTConfig.setReinstallApk(this, GlobalInstance.reinstallApk);
			initConfigValues();
			break;
		case R.id.imgKillProcessBeforeClean:
			GlobalInstance.killProcessBeforeClean = !GlobalInstance.killProcessBeforeClean;
			RTConfig.setKillProcessBeforeClean(this,
					GlobalInstance.killProcessBeforeClean);
			initConfigValues();
			break;
		case R.id.layKillIgnoreList:
			// set kill ignore list
			Intent inIgnore = new Intent(this, MemIgnoreActivity.class);
			startActivity(inIgnore);
			break;
		case R.id.layManualEditHosts:
			// manual edit hosts
			Intent inEditHost = new Intent(this, HostEditActivity.class);
			startActivity(inEditHost);
			break;
		case R.id.layCleanDeprecated:
			// clean deprecated
			Intent inCleanDeprecated = new Intent(this,
					HostDeprecatedActivity.class);
			startActivity(inCleanDeprecated);
			break;
		case R.id.layDeleteAllBackupData:
			// delete all backuped data
			doDeleteAllBackupedData();
			break;
		}
	}

	// [/region]

	// [region] business logic

	private void doDeleteAllBackupedData() {
		AlertDialogEx.showAlertDialogEx(this, getString(R.string.hint),
				getString(R.string.delete_all_backup_data_confirm),
				getString(R.string.ok),
				new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						deleteAllBackupedDataT();
					}
				}, getString(R.string.cancel), null);
	}

	private void deleteAllBackupedDataT() {
		layDeleteAllBackupData.setEnabled(false);
		((TextView) layDeleteAllBackupData
				.findViewById(R.id.tvDeleteAllBackupData))
				.setText(R.string.deleting);
		LogApi.logDeleteAllData();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					((TextView) layDeleteAllBackupData
							.findViewById(R.id.tvDeleteAllBackupData))
							.setText(R.string.delete_all_backup_data);
					layDeleteAllBackupData.setEnabled(true);
					Toast.makeText(SettingsActivity.this,
							R.string.delete_all_backup_data_succ,
							Toast.LENGTH_LONG).show();

				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				ApkUtils.deleteAllBackupData();
				h.sendEmptyMessage(1);

			}
		}).start();
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
		initConfigValues();
	}

	@Override
	public void mappingComp() {

		layAllowDeleteLevel0 = (RelativeLayout) findViewById(R.id.layAllowDeleteLevel0);
		layAlsoDeleteData = (RelativeLayout) findViewById(R.id.layAlsoDeleteData);
		layBackupBeforeDelete = (RelativeLayout) findViewById(R.id.layBackupBeforeDelete);
		layOverrideBackuped = (RelativeLayout) findViewById(R.id.layOverrideBackuped);
		layReinstallApk = (RelativeLayout) findViewById(R.id.layReinstallApk);
		layKillProcessBeforeClean = (RelativeLayout) findViewById(R.id.layKillProcessBeforeClean);
		layKillIgnoreList = (RelativeLayout) findViewById(R.id.layKillIgnoreList);

		layNameServer = (RelativeLayout) findViewById(R.id.layNameServer);
		layManualEditHosts = (RelativeLayout) findViewById(R.id.layManualEditHosts);
		layCleanDeprecated = (RelativeLayout) findViewById(R.id.layCleanDeprecated);
		layDeleteAllBackupData = (RelativeLayout) findViewById(R.id.layDeleteAllBackupData);

		imgAllowDeleteLevel0 = (CheckBox) findViewById(R.id.imgAllowDeleteLevel0);
		imgAlsoDeleteData = (CheckBox) findViewById(R.id.imgAlsoDeleteData);
		imgBackupBeforeDelete = (CheckBox) findViewById(R.id.imgBackupBeforeDelete);
		imgOverrideBackuped = (CheckBox) findViewById(R.id.imgOverrideBackuped);
		imgReinstallApk = (CheckBox) findViewById(R.id.imgReinstallApk);
		imgKillProcessBeforeClean = (CheckBox) findViewById(R.id.imgKillProcessBeforeClean);

		etNameServer = (LineEditText) findViewById(R.id.etNameServer);
	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.settings);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		//
		// tbTitle.setText(getString(R.string.settings));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		btnLeft.setOnClickListener(this);
		// checkbox click
		imgAllowDeleteLevel0.setOnClickListener(this);
		imgAlsoDeleteData.setOnClickListener(this);
		imgBackupBeforeDelete.setOnClickListener(this);
		imgOverrideBackuped.setOnClickListener(this);
		imgReinstallApk.setOnClickListener(this);
		imgKillProcessBeforeClean.setOnClickListener(this);

		// layout click
		layKillIgnoreList.setOnClickListener(this);
		layManualEditHosts.setOnClickListener(this);
		layCleanDeprecated.setOnClickListener(this);
		layDeleteAllBackupData.setOnClickListener(this);

		etNameServer.addTextChangedListener(new TextWatcher() {

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
				GlobalInstance.nameServer = etNameServer.getText().toString();
				RTConfig.setNameServer(SettingsActivity.this,
						GlobalInstance.nameServer);
			}
		});
	}

	private void initConfigValues() {
		etNameServer.setText(GlobalInstance.nameServer);
		imgAllowDeleteLevel0.setChecked(GlobalInstance.allowDeleteLevel0);
		imgAlsoDeleteData.setChecked(GlobalInstance.alsoDeleteData);
		imgBackupBeforeDelete.setChecked(GlobalInstance.backupBeforeDelete);
		imgOverrideBackuped.setChecked(GlobalInstance.overrideBackuped);
		imgReinstallApk.setChecked(GlobalInstance.reinstallApk);
		imgKillProcessBeforeClean
				.setChecked(GlobalInstance.killProcessBeforeClean);
	}

	// [/region]
}
