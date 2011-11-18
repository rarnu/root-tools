package com.snda.root.sapp.manager;

import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.Preference.OnPreferenceChangeListener;
import android.view.KeyEvent;

import com.snda.root.sapp.manager.utils.Misc;

public class SettingsActivity extends PreferenceActivity implements
		OnPreferenceChangeListener {

	CheckBoxPreference chkAllowDelete0;
	CheckBoxPreference chkColorLevel;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		addPreferencesFromResource(R.xml.settings);

		chkAllowDelete0 = (CheckBoxPreference) findPreference(getResources()
				.getString(R.string.key_allow_delete_0));
		chkColorLevel = (CheckBoxPreference) findPreference(getResources()
				.getString(R.string.key_color_level));

		chkAllowDelete0.setChecked(GlobalInstance.allowDeleteLevel0);
		chkColorLevel.setChecked(GlobalInstance.colorLevel);

		chkAllowDelete0.setOnPreferenceChangeListener(this);
		chkColorLevel.setOnPreferenceChangeListener(this);
		
	}

	@Override
	public boolean onPreferenceChange(Preference p, Object value) {
		String key = p.getKey();
		CheckBoxPreference cp = (CheckBoxPreference) p;
		cp.setChecked((Boolean) value);
		if (key.equals(getResources().getString(R.string.key_allow_delete_0))) {
			Misc.setConfig(this, getResources().getString(
					R.string.key_allow_delete_0), cp.isChecked() ? "true"
					: "false");
		} else if (key.equals(getResources()
				.getString(R.string.key_color_level))) {
			Misc.setConfig(this, getResources().getString(
					R.string.key_color_level), cp.isChecked() ? "true"
					: "false");
		}
		return false;
	}
	
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			setResult(RESULT_OK);
			finish();
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

}
