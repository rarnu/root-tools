package com.snda.root.datamgr;

import com.snda.root.datamgr.utils.RootUtils;

import android.app.AlertDialog;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.Preference.OnPreferenceClickListener;

public class SettingsActivity extends PreferenceActivity implements
		OnPreferenceClickListener {

	Preference prefRoot, prefBusybox;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		addPreferencesFromResource(R.xml.settings);

		prefRoot = findPreference("pref_check_root");
		prefBusybox = findPreference("pref_check_busybox");

		prefRoot.setOnPreferenceClickListener(this);
		prefBusybox.setOnPreferenceClickListener(this);

	}

	@Override
	public boolean onPreferenceClick(Preference preference) {
		if (preference.getKey().equals("pref_check_root")) {
			int rootRef = RootUtils.hasRoot();
			int ret = 0;
			switch (rootRef) {
			case 0:
				ret = R.string.root_na;
				break;
			case 1:
				ret = R.string.root_half;
				break;
			case 2:
				ret = R.string.root_full;
				break;
			}
			if (ret != 0) {
				new AlertDialog.Builder(this).setTitle(R.string.hint)
						.setMessage(ret).setPositiveButton(R.string.ok, null)
						.show();
			}

		} else if (preference.getKey().equals("pref_check_busybox")) {
			boolean busyboxRef = RootUtils.hasBusybox();
			int ret = busyboxRef ? R.string.busybox_full : R.string.busybox_na;
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
					ret).setPositiveButton(R.string.ok, null).show();
		}
		return true;
	}
}
