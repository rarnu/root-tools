package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.fragmentactivity.HostDeprecatedActivity;
import com.rarnu.tools.root.fragmentactivity.HostEditActivity;
import com.rarnu.tools.root.fragmentactivity.MemIgnoreActivity;
import com.rarnu.tools.root.receiver.MutaxReceiver;
import com.rarnu.tools.root.receiver.MutaxReceiver.OnReceiveMessage;
import com.rarnu.tools.root.service.CleanBackupService;

public class SettingsFragment extends BaseFragment implements OnClickListener,
		OnReceiveMessage {

	RelativeLayout layAllowDeleteLevel0, layAlsoDeleteData,
			layBackupBeforeDelete, layOverrideBackuped, layReinstallApk,
			layKillProcessBeforeClean, layKillIgnoreList, layNameServer,
			layManualEditHosts, layCleanDeprecated, layDeleteAllBackupData;

	CheckBox imgAllowDeleteLevel0, imgAlsoDeleteData, imgBackupBeforeDelete,
			imgOverrideBackuped, imgReinstallApk, imgKillProcessBeforeClean;

	EditText etNameServer;

	MutaxReceiver receiver;

	@Override
	protected int getBarTitle() {
		return R.string.cat_settings;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.settings_with_path;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		LogApi.logEnterSystemSettings();
	}

	@Override
	public void onResume() {
		super.onResume();
		receiver.register(getActivity());
	}

	@Override
	public void onPause() {
		receiver.unregister(getActivity());
		super.onPause();
	}

	@Override
	protected void initComponents() {
		layAllowDeleteLevel0 = (RelativeLayout) innerView
				.findViewById(R.id.layAllowDeleteLevel0);
		layAlsoDeleteData = (RelativeLayout) innerView
				.findViewById(R.id.layAlsoDeleteData);
		layBackupBeforeDelete = (RelativeLayout) innerView
				.findViewById(R.id.layBackupBeforeDelete);
		layOverrideBackuped = (RelativeLayout) innerView
				.findViewById(R.id.layOverrideBackuped);
		layReinstallApk = (RelativeLayout) innerView
				.findViewById(R.id.layReinstallApk);
		layKillProcessBeforeClean = (RelativeLayout) innerView
				.findViewById(R.id.layKillProcessBeforeClean);
		layKillIgnoreList = (RelativeLayout) innerView
				.findViewById(R.id.layKillIgnoreList);

		layNameServer = (RelativeLayout) innerView
				.findViewById(R.id.layNameServer);
		layManualEditHosts = (RelativeLayout) innerView
				.findViewById(R.id.layManualEditHosts);
		layCleanDeprecated = (RelativeLayout) innerView
				.findViewById(R.id.layCleanDeprecated);
		layDeleteAllBackupData = (RelativeLayout) innerView
				.findViewById(R.id.layDeleteAllBackupData);

		imgAllowDeleteLevel0 = (CheckBox) innerView
				.findViewById(R.id.imgAllowDeleteLevel0);
		imgAlsoDeleteData = (CheckBox) innerView
				.findViewById(R.id.imgAlsoDeleteData);
		imgBackupBeforeDelete = (CheckBox) innerView
				.findViewById(R.id.imgBackupBeforeDelete);
		imgOverrideBackuped = (CheckBox) innerView
				.findViewById(R.id.imgOverrideBackuped);
		imgReinstallApk = (CheckBox) innerView
				.findViewById(R.id.imgReinstallApk);
		imgKillProcessBeforeClean = (CheckBox) innerView
				.findViewById(R.id.imgKillProcessBeforeClean);

		etNameServer = (EditText) innerView.findViewById(R.id.etNameServer);

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
				RTConfig.setNameServer(getActivity(), GlobalInstance.nameServer);
			}
		});

		receiver = new MutaxReceiver(Actions.ACTION_CLEANING_BACKUP, null, null);
		receiver.setOnReceiveMessage(this);
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_settings;
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.imgAllowDeleteLevel0:
			GlobalInstance.allowDeleteLevel0 = !GlobalInstance.allowDeleteLevel0;
			RTConfig.setAllowDeleteLevel0(getActivity(),
					GlobalInstance.allowDeleteLevel0);
			initConfigValues();
			break;
		case R.id.imgAlsoDeleteData:
			GlobalInstance.alsoDeleteData = !GlobalInstance.alsoDeleteData;
			RTConfig.setAlsoDeleteData(getActivity(),
					GlobalInstance.alsoDeleteData);
			initConfigValues();
			break;
		case R.id.imgBackupBeforeDelete:
			GlobalInstance.backupBeforeDelete = !GlobalInstance.backupBeforeDelete;
			RTConfig.setBackupBeforeDelete(getActivity(),
					GlobalInstance.backupBeforeDelete);
			initConfigValues();
			break;
		case R.id.imgOverrideBackuped:
			GlobalInstance.overrideBackuped = !GlobalInstance.overrideBackuped;
			RTConfig.setOverrideBackuped(getActivity(),
					GlobalInstance.overrideBackuped);
			initConfigValues();
			break;
		case R.id.imgReinstallApk:
			GlobalInstance.reinstallApk = !GlobalInstance.reinstallApk;
			RTConfig.setReinstallApk(getActivity(), GlobalInstance.reinstallApk);
			initConfigValues();
			break;
		case R.id.imgKillProcessBeforeClean:
			GlobalInstance.killProcessBeforeClean = !GlobalInstance.killProcessBeforeClean;
			RTConfig.setKillProcessBeforeClean(getActivity(),
					GlobalInstance.killProcessBeforeClean);
			initConfigValues();
			break;
		case R.id.layKillIgnoreList:
			// set kill ignore list
			Intent inIgnore = new Intent(getActivity(), MemIgnoreActivity.class);
			startActivity(inIgnore);
			break;
		case R.id.layManualEditHosts:
			// manual edit hosts
			Intent inEditHost = new Intent(getActivity(),
					HostEditActivity.class);
			startActivity(inEditHost);
			break;
		case R.id.layCleanDeprecated:
			// clean deprecated
			Intent inCleanDeprecated = new Intent(getActivity(),
					HostDeprecatedActivity.class);
			startActivity(inCleanDeprecated);
			break;
		case R.id.layDeleteAllBackupData:
			// delete all backuped data
			doDeleteAllBackupedData();
			break;
		}

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

	private void deleteAllBackupedDataT() {
		setCleaningState(true);
		Intent inCleanBackupService = new Intent(getActivity(),
				CleanBackupService.class);
		inCleanBackupService.putExtra("command", "clean-backup");
		inCleanBackupService.putExtra("id", RTConsts.NOTIFY_ID_CLEAN_BACKUP);
		inCleanBackupService.putExtra("title", R.string.delete_all_backup_data);
		inCleanBackupService.putExtra("desc",
				R.string.delete_all_backup_data_succ);
		inCleanBackupService.putExtra("proc_id",
				RTConsts.NOTIFY_PROC_CLEAN_BACKUP);
		inCleanBackupService.putExtra("proc_title",
				R.string.delete_all_backup_data);
		inCleanBackupService.putExtra("proc_desc",
				R.string.deleting);
		getActivity().startService(inCleanBackupService);
	}

	private void doDeleteAllBackupedData() {
		AlertDialogEx.showAlertDialogEx(getActivity(),
				getString(R.string.hint),
				getString(R.string.delete_all_backup_data_confirm),
				getString(R.string.ok),
				new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						deleteAllBackupedDataT();
					}
				}, getString(R.string.cancel), null);
	}

	@Override
	protected void initLogic() {
		initConfigValues();
	}

	@Override
	public void onStateChange(boolean operating) {
		if (!operating) {
			Intent inCleanBackupService = new Intent(getActivity(),
					CleanBackupService.class);
			getActivity().stopService(inCleanBackupService);
		}
		setCleaningState(operating);

	}

	private void setCleaningState(boolean operating) {
		layDeleteAllBackupData.setEnabled(!operating);
		((TextView) layDeleteAllBackupData
				.findViewById(R.id.tvDeleteAllBackupData))
				.setText(operating ? R.string.deleting
						: R.string.delete_all_backup_data);
	}

	@Override
	public void onProgress(String name, int position, int total) {

	}

	@Override
	public void onMutaxMessage(boolean operating) {

	}

}
