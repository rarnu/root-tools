package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceClickListener;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup.LayoutParams;
import android.widget.CheckBox;
import android.widget.EditText;

import com.rarnu.devlib.base.inner.InnerPreferenceFragment;
import com.rarnu.devlib.component.CheckBoxPreferenceEx;
import com.rarnu.devlib.component.PreferenceEx;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.fragmentactivity.CustomCleanManagerActivity;
import com.rarnu.tools.root.fragmentactivity.HostDeprecatedActivity;
import com.rarnu.tools.root.fragmentactivity.HostEditActivity;
import com.rarnu.tools.root.fragmentactivity.MemIgnoreActivity;
import com.rarnu.tools.root.receiver.MutaxReceiver;
import com.rarnu.tools.root.receiver.MutaxReceiver.OnReceiveMessage;
import com.rarnu.tools.root.service.CleanBackupService;

public class SettingsFragment extends InnerPreferenceFragment implements
		OnReceiveMessage, OnClickListener, OnPreferenceClickListener {

	CheckBoxPreferenceEx prefAllowDeleteLevel0, prefAlsoDeleteData,
			prefBackupBeforeDelete, prefOverrideBackuped, prefReinstallApk,
			prefKillProcessBeforeClean;

	PreferenceEx prefKillIgnoreList, prefNameServer, prefManualEditHosts,
			prefCleanDeprecated, prefDeleteAllBackupData, prefCustomAppClean;

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

		prefAllowDeleteLevel0 = (CheckBoxPreferenceEx) findPreference(getString(R.string.id_allow_delete_level_0));
		prefAlsoDeleteData = (CheckBoxPreferenceEx) findPreference(getString(R.string.id_also_delete_data));
		prefBackupBeforeDelete = (CheckBoxPreferenceEx) findPreference(getString(R.string.id_backup_before_delete));
		prefOverrideBackuped = (CheckBoxPreferenceEx) findPreference(getString(R.string.id_override_backuped));
		prefReinstallApk = (CheckBoxPreferenceEx) findPreference(getString(R.string.id_reinstall_apk));
		prefKillProcessBeforeClean = (CheckBoxPreferenceEx) findPreference(getString(R.string.id_kill_process_before_clean));

		prefKillIgnoreList = (PreferenceEx) findPreference(getString(R.string.id_kill_ignore_list));
		prefNameServer = (PreferenceEx) findPreference(getString(R.string.id_name_server));
		prefManualEditHosts = (PreferenceEx) findPreference(getString(R.string.id_manual_edit_hosts));
		prefCleanDeprecated = (PreferenceEx) findPreference(getString(R.string.id_clean_deprecated_hosts));
		prefDeleteAllBackupData = (PreferenceEx) findPreference(getString(R.string.id_delete_all_backup_data));
		prefCustomAppClean = (PreferenceEx) findPreference(getString(R.string.id_custom_app_clean));

		receiver = new MutaxReceiver(Actions.ACTION_CLEANING_BACKUP, null, null);

	}

	@Override
	protected void initMenu(Menu menu) {

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
		inCleanBackupService.putExtra("proc_desc", R.string.deleting);
		getActivity().startService(inCleanBackupService);
	}

	private void doDeleteAllBackupedData() {
		new AlertDialog.Builder(getActivity())
				.setTitle(R.string.hint)
				.setMessage(R.string.delete_all_backup_data_confirm)
				.setPositiveButton(R.string.ok,
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface dialog,
									int which) {
								deleteAllBackupedDataT();

							}
						}).setNegativeButton(R.string.cancel, null).show();

	}

	@Override
	protected void initLogic() {
		initConfigValues();
	}

	private void initConfigValues() {

		prefAllowDeleteLevel0.setStateChecked(GlobalInstance.allowDeleteLevel0);
		prefAlsoDeleteData.setStateChecked(GlobalInstance.alsoDeleteData);
		prefBackupBeforeDelete
				.setStateChecked(GlobalInstance.backupBeforeDelete);
		prefOverrideBackuped.setStateChecked(GlobalInstance.overrideBackuped);
		prefReinstallApk.setStateChecked(GlobalInstance.reinstallApk);
		prefKillProcessBeforeClean
				.setStateChecked(GlobalInstance.killProcessBeforeClean);
		prefNameServer.setSummary(GlobalInstance.nameServer);

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
		prefDeleteAllBackupData.setEnabled(!operating);
		prefDeleteAllBackupData.setTitle(operating ? R.string.deleting
				: R.string.delete_all_backup_data);
	}

	@Override
	public void onProgress(String name, int position, int total) {

	}

	@Override
	public void onMutaxMessage(boolean operating) {

	}

	@Override
	public void onClick(View v) {
		String key = (String) v.getTag();
		CheckBox chk = (CheckBox) v;
		if (key.equals(getString(R.string.id_allow_delete_level_0))) {
			GlobalInstance.allowDeleteLevel0 = chk.isChecked();
			RTConfig.setAllowDeleteLevel0(getActivity(),
					GlobalInstance.allowDeleteLevel0);
			initConfigValues();
		} else if (key.equals(getString(R.string.id_also_delete_data))) {
			GlobalInstance.alsoDeleteData = chk.isChecked();
			RTConfig.setAlsoDeleteData(getActivity(),
					GlobalInstance.alsoDeleteData);
			initConfigValues();
		} else if (key.equals(getString(R.string.id_backup_before_delete))) {
			GlobalInstance.backupBeforeDelete = chk.isChecked();
			RTConfig.setBackupBeforeDelete(getActivity(),
					GlobalInstance.backupBeforeDelete);
			initConfigValues();
		} else if (key.equals(getString(R.string.id_override_backuped))) {
			GlobalInstance.overrideBackuped = chk.isChecked();
			RTConfig.setOverrideBackuped(getActivity(),
					GlobalInstance.overrideBackuped);
			initConfigValues();
		} else if (key.equals(getString(R.string.id_reinstall_apk))) {
			GlobalInstance.reinstallApk = chk.isChecked();
			RTConfig.setReinstallApk(getActivity(), GlobalInstance.reinstallApk);
			initConfigValues();
		} else if (key.equals(getString(R.string.id_kill_process_before_clean))) {
			GlobalInstance.killProcessBeforeClean = chk.isChecked();
			RTConfig.setKillProcessBeforeClean(getActivity(),
					GlobalInstance.killProcessBeforeClean);
			initConfigValues();
		}

	}

	@Override
	public boolean onPreferenceClick(Preference preference) {
		String key = preference.getKey();
		if (key.equals(getString(R.string.id_kill_ignore_list))) {
			Intent inIgnore = new Intent(getActivity(), MemIgnoreActivity.class);
			startActivity(inIgnore);
		} else if (key.equals(getString(R.string.id_name_server))) {
			changeDnsServer();
		} else if (key.equals(getString(R.string.id_manual_edit_hosts))) {
			Intent inEditHost = new Intent(getActivity(),
					HostEditActivity.class);
			startActivity(inEditHost);
		} else if (key.equals(getString(R.string.id_clean_deprecated_hosts))) {
			Intent inCleanDeprecated = new Intent(getActivity(),
					HostDeprecatedActivity.class);
			startActivity(inCleanDeprecated);
		} else if (key.equals(getString(R.string.id_delete_all_backup_data))) {
			doDeleteAllBackupedData();
		} else if (key.equals(getString(R.string.id_custom_app_clean))) {
			// manage custom app clean
			Intent inCustomClean = new Intent(getActivity(),
					CustomCleanManagerActivity.class);
			startActivity(inCustomClean);
		}
		return true;
	}

	private void changeDnsServer() {
		final EditText etNameServer = new EditText(getActivity());
		etNameServer.setLayoutParams(new LayoutParams(
				LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		etNameServer.setText(GlobalInstance.nameServer);
		new AlertDialog.Builder(getActivity())
				.setTitle(R.string.name_server)
				.setMessage(R.string.name_server_hint)
				.setView(etNameServer)
				.setPositiveButton(R.string.ok,
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface dialog,
									int which) {
								GlobalInstance.nameServer = etNameServer
										.getText().toString();
								RTConfig.setNameServer(getActivity(),
										GlobalInstance.nameServer);
								initConfigValues();

							}
						}).setNegativeButton(R.string.cancel, null).show();

	}

	@Override
	protected void initEvents() {
		prefAllowDeleteLevel0.setOnCheckboxClickListener(this);
		prefAlsoDeleteData.setOnCheckboxClickListener(this);
		prefBackupBeforeDelete.setOnCheckboxClickListener(this);
		prefOverrideBackuped.setOnCheckboxClickListener(this);
		prefReinstallApk.setOnCheckboxClickListener(this);
		prefKillProcessBeforeClean.setOnCheckboxClickListener(this);
		prefKillIgnoreList.setOnPreferenceClickListener(this);
		prefNameServer.setOnPreferenceClickListener(this);
		prefManualEditHosts.setOnPreferenceClickListener(this);
		prefCleanDeprecated.setOnPreferenceClickListener(this);
		prefDeleteAllBackupData.setOnPreferenceClickListener(this);
		prefCustomAppClean.setOnPreferenceClickListener(this);
		receiver.setOnReceiveMessage(this);
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected int getPreferenceLayoutId() {
		return R.xml.settings;
	}

	@Override
	protected String getCustomTitle() {
		return null;
	}

}
