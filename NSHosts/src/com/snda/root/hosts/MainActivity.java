package com.snda.root.hosts;

import com.snda.root.hosts.root.RootUtils;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.Preference.OnPreferenceClickListener;

public class MainActivity extends PreferenceActivity implements OnPreferenceClickListener {

	Preference pHosts, pLookup, pOption, pAbout;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        if (RootUtils.hasRoot() == 0) {
        	new AlertDialog.Builder(this)
        		.setTitle(R.string.c_hint)
        		.setMessage(R.string.c_noroot)
        		.setPositiveButton(R.string.c_ok, new DialogInterface.OnClickListener() {
					
					@Override
					public void onClick(DialogInterface dialog, int which) {
						finish();
					}
				})
        		.show();
        	return;
        }
        
        addPreferencesFromResource(R.xml.main);
        
        pHosts = findPreference(getResources().getString(R.string.key_hosts));
        pLookup = findPreference(getResources().getString(R.string.key_ns));
        pOption = findPreference(getResources().getString(R.string.key_option));
        pAbout = findPreference(getResources().getString(R.string.key_about));
        
        pHosts.setOnPreferenceClickListener(this);
        pLookup.setOnPreferenceClickListener(this);
        pOption.setOnPreferenceClickListener(this);
        pAbout.setOnPreferenceClickListener(this);
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
			// TODO: options
		} else if (key.equals(getResources().getString(R.string.key_about))) {
			// TODO: help
		}
		
		return false;
	}
}