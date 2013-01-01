package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceFragment;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.comp.PreferenceEx;
import com.rarnu.tools.root.fragmentactivity.AboutActivity;
import com.rarnu.tools.root.fragmentactivity.BusyboxActivity;
import com.rarnu.tools.root.fragmentactivity.SysappMainActivity;
import com.rarnu.tools.root.fragmentactivity.UserFeedbackActivity;
import com.rarnu.tools.root.utils.BusyboxUtils;
import com.rarnu.tools.root.utils.MiscUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class MainFragment extends PreferenceFragment implements
		OnPreferenceClickListener {

	PreferenceEx prefSysApp, prefSysAppEnabled, prefComponent, prefRoot,
			prefHtcRom;
	PreferenceEx prefBackup;
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
		prefSysApp.setOnPreferenceClickListener(this);
		prefSysAppEnabled.setOnPreferenceClickListener(this);
		prefComponent.setOnPreferenceClickListener(this);
		prefRoot.setOnPreferenceClickListener(this);
		prefHtcRom.setOnPreferenceClickListener(this);

		prefBackup.setOnPreferenceClickListener(this);

		prefCleanMemory.setOnPreferenceClickListener(this);
		prefCleanCache.setOnPreferenceClickListener(this);
		prefCleanDalvik.setOnPreferenceClickListener(this);

		prefHosts.setOnPreferenceClickListener(this);
		prefScanMedia.setOnPreferenceClickListener(this);
		prefNetworkState.setOnPreferenceClickListener(this);
		prefReboot.setOnPreferenceClickListener(this);

		prefFeedback.setOnPreferenceClickListener(this);
		prefRecommand.setOnPreferenceClickListener(this);
		prefAbout.setOnPreferenceClickListener(this);
		
		prefSettings.setOnPreferenceClickListener(this);
	}

	@Override
	public boolean onPreferenceClick(Preference preference) {

		// system
		if (preference.getKey().equals(getString(R.string.id_sysapp))) {
			GlobalInstance.currentFragment = 1;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					SysappMainActivity.class), GlobalFragment.fSysapp);

		} else if (preference.getKey().equals(
				getString(R.string.id_sysappenabled))) {
			GlobalInstance.currentFragment = 2;

		} else if (preference.getKey().equals(getString(R.string.id_component))) {
			GlobalInstance.currentFragment = 3;

		} else if (preference.getKey().equals(getString(R.string.id_root))) {
			GlobalInstance.currentFragment = 4;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					BusyboxActivity.class), GlobalFragment.fBusybox);

		} else if (preference.getKey().equals(getString(R.string.id_cleanhtc))) {
			GlobalInstance.currentFragment = 5;

		}
		// backup
		else if (preference.getKey().equals(getString(R.string.id_backup))) {
			GlobalInstance.currentFragment = 6;
		}

		// memory
		else if (preference.getKey().equals(getString(R.string.id_cleanmemory))) {
			GlobalInstance.currentFragment = 7;

		} else if (preference.getKey()
				.equals(getString(R.string.id_cleancache))) {
			GlobalInstance.currentFragment = 8;

		} else if (preference.getKey().equals(
				getString(R.string.id_cleandalvik))) {
			MiscUtils.doCleanDalvik(getActivity(), getView(), prefCleanDalvik);
		}

		// other
		else if (preference.getKey().equals(getString(R.string.id_hosts))) {
			GlobalInstance.currentFragment = 9;

		} else if (preference.getKey().equals(getString(R.string.id_scanmedia))) {
			MiscUtils.doScanMedia(getActivity());
		} else if (preference.getKey().equals(getString(R.string.id_network))) {
			MiscUtils.showNetworkStatus(getActivity());
		} else if (preference.getKey().equals(getString(R.string.id_reboot))) {
			MiscUtils.doReboot(getActivity());
		}

		// support
		else if (preference.getKey().equals(getString(R.string.id_feedback))) {
			GlobalInstance.currentFragment = 10;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					UserFeedbackActivity.class), GlobalFragment.fFeedback);
		} else if (preference.getKey().equals(getString(R.string.id_recommand))) {
			GlobalInstance.currentFragment = 11;

		} else if (preference.getKey().equals(getString(R.string.id_about))) {
			GlobalInstance.currentFragment = 12;
			GlobalFragment.showContent(getActivity(), new Intent(getActivity(),
					AboutActivity.class), GlobalFragment.fAbout);
		}
		
		//
		else if (preference.getKey().equals(getString(R.string.id_settings))) {
			GlobalInstance.currentFragment = 13;
			
		}

		return true;
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
		prefCleanCache.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefHosts.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefReboot.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
	}
}
