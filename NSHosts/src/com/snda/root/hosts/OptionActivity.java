package com.snda.root.hosts;

import org.apache.http.protocol.HTTP;

import com.snda.root.hosts.utils.FileUtils;
import com.snda.root.hosts.utils.NetworkUtils;

import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.preference.CheckBoxPreference;
import android.preference.EditTextPreference;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.widget.Toast;

public class OptionActivity extends PreferenceActivity implements
		OnPreferenceChangeListener, OnPreferenceClickListener {

	EditTextPreference pNameServer;
	CheckBoxPreference pAutoSelect;
	Preference pUpdateSites;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		addPreferencesFromResource(R.xml.option);

		pNameServer = (EditTextPreference) findPreference(getResources()
				.getString(R.string.key_nameserver));
		pAutoSelect = (CheckBoxPreference) findPreference(getResources()
				.getString(R.string.key_autoselect));
		pUpdateSites = findPreference(getResources().getString(
				R.string.key_updatesites));
		pNameServer.setOnPreferenceChangeListener(this);
		pAutoSelect.setOnPreferenceChangeListener(this);
		pUpdateSites.setOnPreferenceClickListener(this);
	}

	@Override
	public boolean onPreferenceChange(Preference preference, Object newValue) {
		String key = preference.getKey();
		if (key.equals(getResources().getString(R.string.key_nameserver))) {
			GlobalInstance.nameServer = (String) newValue;
		} else if (key
				.equals(getResources().getString(R.string.key_autoselect))) {
			GlobalInstance.autoSelect = (Boolean) newValue;
		}
		return true;
	}

	@Override
	public boolean onPreferenceClick(Preference preference) {
		String key = preference.getKey();
		if (key.equals(getResources().getString(R.string.key_updatesites))) {
			updateComminSitesT();
		}
		return false;
	}

	private void updateComminSitesT() {

		final ProgressDialog pd = new ProgressDialog(this);
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					pd.dismiss();
					Toast.makeText(OptionActivity.this, R.string.c_updatesiteok, Toast.LENGTH_LONG).show();
				} else if (msg.what == 2) {
					pd.dismiss();
					Toast.makeText(OptionActivity.this, R.string.c_updatesitefail, Toast.LENGTH_LONG).show();
				}
				super.handleMessage(msg);
			}
		};
		final Thread t = new Thread(new Runnable() {

			@Override
			public void run() {
				int what = 1;
				try {
					String s = NetworkUtils.CallGet(
							GlobalInstance.common_site_url, "", HTTP.UTF_8);
					FileUtils.rewriteFile(GlobalInstance.app_dir
							+ GlobalInstance.domain_filename, s);

				} catch (Exception e) {
					what = 2;
				}

				h.sendEmptyMessage(what);

			}
		});

		pd.setMessage(getResources().getString(R.string.c_updatingsites));
		pd.setCancelable(true);
		pd.setOnCancelListener(new OnCancelListener() {

			@Override
			public void onCancel(DialogInterface dialog) {
				if (t.isAlive()) {
					t.interrupt();
				}
			}
		});
		pd.show();
		t.start();

	}
}
