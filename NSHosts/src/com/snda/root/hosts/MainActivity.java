package com.snda.root.hosts;

import android.content.Intent;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.Preference.OnPreferenceClickListener;

public class MainActivity extends PreferenceActivity implements OnPreferenceClickListener {

	Preference pHosts, pLookup, pAbout;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.main);
        
        pHosts = findPreference(getResources().getString(R.string.key_hosts));
        pLookup = findPreference(getResources().getString(R.string.key_ns));
        pAbout = findPreference(getResources().getString(R.string.key_about));
        
        pHosts.setOnPreferenceClickListener(this);
        pLookup.setOnPreferenceClickListener(this);
        pAbout.setOnPreferenceClickListener(this);
    }

	public boolean onPreferenceClick(Preference p) {
		String key = p.getKey(); 
		
		if (key.equals(getResources().getString(R.string.key_hosts))) {
			// TODO: /etc/hosts/
		} else if (key.equals(getResources().getString(R.string.key_ns))) {
			// lookup
			Intent inLookup = new Intent(this, LookupActivity.class);
			startActivity(inLookup);
		} else if (key.equals(getResources().getString(R.string.key_about))) {
			// TODO: help
		}
		
		return false;
	}
}