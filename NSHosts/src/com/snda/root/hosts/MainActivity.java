package com.snda.root.hosts;

import java.io.File;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.Preference.OnPreferenceClickListener;

import com.snda.root.hosts.root.RootUtils;
import com.snda.root.hosts.utils.ConfigUtils;

public class MainActivity extends PreferenceActivity implements
		OnPreferenceClickListener {

	Preference pHosts, pLookup, pOption, pAbout;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (RootUtils.hasRoot() == 0) {
			new AlertDialog.Builder(this).setTitle(R.string.c_hint).setMessage(
					R.string.c_noroot).setPositiveButton(R.string.c_ok,
					new DialogInterface.OnClickListener() {

						@Override
						public void onClick(DialogInterface dialog, int which) {
							finish();
						}
					}).show();
			return;
		}

		addPreferencesFromResource(R.xml.main);

		File fAppDir = new File(GlobalInstance.app_dir);
		if (!fAppDir.exists()) {
			fAppDir.mkdirs();
		}

		pHosts = findPreference(getResources().getString(R.string.key_hosts));
		pLookup = findPreference(getResources().getString(R.string.key_ns));
		pOption = findPreference(getResources().getString(R.string.key_option));
		pAbout = findPreference(getResources().getString(R.string.key_about));

		pHosts.setOnPreferenceClickListener(this);
		pLookup.setOnPreferenceClickListener(this);
		pOption.setOnPreferenceClickListener(this);
		pAbout.setOnPreferenceClickListener(this);

		// load config
		GlobalInstance.nameServer = ConfigUtils.getConfig(this, getResources()
				.getString(R.string.key_nameserver), getResources().getString(
				R.string.default_nameserver));
		GlobalInstance.autoSelect = ConfigUtils.getConfig(this, getResources()
				.getString(R.string.key_autoselect), true);
	}

	public boolean onPreferenceClick(Preference p) {
		String key = p.getKey();

		if (key.equals(getResources().getString(R.string.key_hosts))) {
			// /etc/hosts/
			Intent inHosts = new Intent(this, HostsActivity.class);
			inHosts.putExtra("mode", 0);
			startActivity(inHosts);
		} else if (key.equals(getResources().getString(R.string.key_ns))) {
			// lookup
			Intent inLookup = new Intent(this, LookupActivity.class);
			startActivity(inLookup);
		} else if (key.equals(getResources().getString(R.string.key_option))) {
			// options
			Intent inOption = new Intent(this, OptionActivity.class);
			startActivity(inOption);
		} else if (key.equals(getResources().getString(R.string.key_about))) {
			// help
			Intent inHelp = new Intent(this, HelpActivity.class);
			startActivity(inHelp);
		}

		return false;
	}
}