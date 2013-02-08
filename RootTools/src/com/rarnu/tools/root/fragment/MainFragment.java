package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.preference.PreferenceFragment;
import android.view.View;
import android.view.View.OnClickListener;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.comp.PreferenceEx;
import com.rarnu.tools.root.fragmentactivity.AboutActivity;
import com.rarnu.tools.root.fragmentactivity.BusyboxActivity;
import com.rarnu.tools.root.fragmentactivity.CleanCacheMainActivity;
import com.rarnu.tools.root.fragmentactivity.CompMainActivity;
import com.rarnu.tools.root.fragmentactivity.DataBackupActivity;
import com.rarnu.tools.root.fragmentactivity.DataRestoreActivity;
import com.rarnu.tools.root.fragmentactivity.EnableappMainActivity;
import com.rarnu.tools.root.fragmentactivity.HostMainActivity;
import com.rarnu.tools.root.fragmentactivity.HtcRomActivity;
import com.rarnu.tools.root.fragmentactivity.MemMainActivity;
import com.rarnu.tools.root.fragmentactivity.RecommandActivity;
import com.rarnu.tools.root.fragmentactivity.SettingsActivity;
import com.rarnu.tools.root.fragmentactivity.SysappMainActivity;
import com.rarnu.tools.root.fragmentactivity.UserFeedbackActivity;
import com.rarnu.tools.root.utils.BusyboxUtils;
import com.rarnu.tools.root.utils.MiscUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class MainFragment extends PreferenceFragment implements OnClickListener {

	PreferenceEx prefSysApp, prefSysAppEnabled, prefComponent, prefRoot,
			prefHtcRom;
	PreferenceEx prefBackup, prefRestore;
	PreferenceEx prefCleanMemory, prefCleanCache, prefCleanDalvik;
	PreferenceEx prefHosts, prefScanMedia, prefNetworkState, prefReboot;
	PreferenceEx prefFeedback, prefRecommand, prefAbout;
	PreferenceEx prefSettings;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		addPreferencesFromResource(R.xml.main);
		mappingComponents();
		initEvents();
		showFunctionalEnabledTags();
	}

	private void mappingComponents() {
		prefSysApp = (PreferenceEx) findPreference(getString(R.string.id_sysapp));
		prefSysAppEnabled = (PreferenceEx) findPreference(getString(R.string.id_sysappenabled));
		prefComponent = (PreferenceEx) findPreference(getString(R.string.id_component));
		prefRoot = (PreferenceEx) findPreference(getString(R.string.id_root));
		prefHtcRom = (PreferenceEx) findPreference(getString(R.string.id_cleanhtc));

		prefBackup = (PreferenceEx) findPreference(getString(R.string.id_backup));
		prefRestore = (PreferenceEx) findPreference(getString(R.string.id_restore));

		prefCleanMemory = (PreferenceEx) findPreference(getString(R.string.id_cleanmemory));
		prefCleanCache = (PreferenceEx) findPreference(getString(R.string.id_cleancache));
		prefCleanDalvik = (PreferenceEx) findPreference(getString(R.string.id_cleandalvik));

		prefHosts = (PreferenceEx) findPreference(getString(R.string.id_hosts));
		prefScanMedia = (PreferenceEx) findPreference(getString(R.string.id_scanmedia));
		prefNetworkState = (PreferenceEx) findPreference(getString(R.string.id_network));
		prefReboot = (PreferenceEx) findPreference(getString(R.string.id_reboot));

		prefFeedback = (PreferenceEx) findPreference(getString(R.string.id_feedback));
		prefRecommand = (PreferenceEx) findPreference(getString(R.string.id_recommand));
		prefAbout = (PreferenceEx) findPreference(getString(R.string.id_about));

		prefSettings = (PreferenceEx) findPreference(getString(R.string.id_settings));

	}

	private void initEvents() {

		prefSysApp.setInnerTag(R.string.id_sysapp);
		prefSysApp.setOnInnerClick(this);
		prefSysAppEnabled.setInnerTag(R.string.id_sysappenabled);
		prefSysAppEnabled.setOnInnerClick(this);
		prefComponent.setInnerTag(R.string.id_component);
		prefComponent.setOnInnerClick(this);
		prefRoot.setInnerTag(R.string.id_root);
		prefRoot.setOnInnerClick(this);
		prefHtcRom.setInnerTag(R.string.id_cleanhtc);
		prefHtcRom.setOnInnerClick(this);

		prefBackup.setInnerTag(R.string.id_backup);
		prefBackup.setOnInnerClick(this);
		prefRestore.setInnerTag(R.string.id_restore);
		prefRestore.setOnInnerClick(this);

		prefCleanMemory.setInnerTag(R.string.id_cleanmemory);
		prefCleanMemory.setOnInnerClick(this);
		prefCleanCache.setInnerTag(R.string.id_cleancache);
		prefCleanCache.setOnInnerClick(this);
		prefCleanDalvik.setInnerTag(R.string.id_cleandalvik);
		prefCleanDalvik.setOnInnerClick(this);

		prefHosts.setInnerTag(R.string.id_hosts);
		prefHosts.setOnInnerClick(this);
		prefScanMedia.setInnerTag(R.string.id_scanmedia);
		prefScanMedia.setOnInnerClick(this);
		prefNetworkState.setInnerTag(R.string.id_network);
		prefNetworkState.setOnInnerClick(this);
		prefReboot.setInnerTag(R.string.id_reboot);
		prefReboot.setOnInnerClick(this);

		prefFeedback.setInnerTag(R.string.id_feedback);
		prefFeedback.setOnInnerClick(this);
		prefRecommand.setInnerTag(R.string.id_recommand);
		prefRecommand.setOnInnerClick(this);
		prefAbout.setInnerTag(R.string.id_about);
		prefAbout.setOnInnerClick(this);

		prefSettings.setInnerTag(R.string.id_settings);
		prefSettings.setOnInnerClick(this);
	}

	public void showFunctionalEnabledTags() {

		boolean isRooted = RootUtils.hasSu();
		boolean isWrongRooted = RootUtils.isWrongRoot();
		if (isWrongRooted) {
			isRooted = false;
		}

		prefSysApp.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefSysAppEnabled.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefComponent.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefRoot.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefHtcRom.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefBackup.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefRestore.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefCleanMemory.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefCleanCache.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefCleanDalvik.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefHosts.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefReboot.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);

		prefScanMedia.setStatus(PreferenceEx.STATE_NORMAL);
		prefNetworkState.setStatus(PreferenceEx.STATE_NORMAL);
		prefFeedback.setStatus(PreferenceEx.STATE_NORMAL);
		prefRecommand.setStatus(PreferenceEx.STATE_NORMAL);
		prefAbout.setStatus(PreferenceEx.STATE_NORMAL);

		if (isRooted) {
			showBusyboxTag();
		}
	}

	private void showBusyboxTag() {
		boolean ready = BusyboxUtils.isSuBusyboxReady();

		prefSysApp.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefSysAppEnabled.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefRoot.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefBackup.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefRestore.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefCleanCache.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefHosts.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefReboot.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
	}

	@Override
	public void onClick(View v) {
		int tag = (Integer) v.getTag();

		switch (tag) {
		case R.string.id_sysapp:
			GlobalInstance.currentFragment = 1;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					SysappMainActivity.class), GlobalFragment.fSysapp);
			break;
		case R.string.id_sysappenabled:
			GlobalInstance.currentFragment = 2;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					EnableappMainActivity.class), GlobalFragment.fEnableapp);
			break;
		case R.string.id_component:
			GlobalInstance.currentFragment = 3;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					CompMainActivity.class), GlobalFragment.fComp);
			break;
		case R.string.id_root:
			GlobalInstance.currentFragment = 4;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					BusyboxActivity.class), GlobalFragment.fBusybox);
			break;
		case R.string.id_cleanhtc:
			GlobalInstance.currentFragment = 5;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					HtcRomActivity.class), GlobalFragment.fHtcRom);
			break;
		case R.string.id_backup:
			GlobalInstance.currentFragment = 6;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					DataBackupActivity.class), GlobalFragment.fBackup);
			break;
		case R.string.id_restore:
			GlobalInstance.currentFragment = 14;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					DataRestoreActivity.class), GlobalFragment.fRestore);
			break;
		case R.string.id_cleanmemory:
			GlobalInstance.currentFragment = 7;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					MemMainActivity.class), GlobalFragment.fMem);
			break;
		case R.string.id_cleancache:
			GlobalInstance.currentFragment = 8;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					CleanCacheMainActivity.class), GlobalFragment.fCleanCache);
			break;
		case R.string.id_cleandalvik:
			MiscUtils.doCleanDalvik(getActivity(), getView(), prefCleanDalvik);
			break;
		case R.string.id_hosts:
			GlobalInstance.currentFragment = 9;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					HostMainActivity.class), GlobalFragment.fHost);
			break;
		case R.string.id_scanmedia:
			MiscUtils.doScanMedia(getActivity());
			break;
		case R.string.id_network:
			MiscUtils.showNetworkStatus(getActivity());
			break;
		case R.string.id_reboot:
			MiscUtils.doReboot(getActivity());
			break;
		case R.string.id_feedback:
			GlobalInstance.currentFragment = 10;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					UserFeedbackActivity.class), GlobalFragment.fFeedback);
			break;
		case R.string.id_recommand:
			GlobalInstance.currentFragment = 11;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					RecommandActivity.class), GlobalFragment.fRecommand);
			break;
		case R.string.id_about:
			GlobalInstance.currentFragment = 12;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					AboutActivity.class), GlobalFragment.fAbout);
			break;
		case R.string.id_settings:
			GlobalInstance.currentFragment = 13;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					SettingsActivity.class), GlobalFragment.fSettings);
			break;
		}

	}

}
