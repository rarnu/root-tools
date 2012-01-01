package com.snda.root.bcm.utils;

import android.content.ComponentName;
import android.content.pm.PackageManager;
import android.util.Log;

import com.snda.root.bcm.GlobalInstance;

public class ComponentUtils {

	public static void doEnabledReceiver(ComponentName receiverName) {

		// pm enable com.melodis.midomiMusicIdentifier/com.google.ads.InstallReceiver
		try {
			GlobalInstance.pm.setComponentEnabledSetting(receiverName,
					PackageManager.COMPONENT_ENABLED_STATE_ENABLED,
					PackageManager.DONT_KILL_APP);
		} catch (Exception e) {
			Log.e("bcm-error", e.getMessage());
		}

	}

	public static void doDisableReceiver(ComponentName receiverName) {
		
		// pm disable com.melodis.midomiMusicIdentifier/com.google.ads.InstallReceiver
		GlobalInstance.pm.setComponentEnabledSetting(receiverName,
				PackageManager.COMPONENT_ENABLED_STATE_DISABLED,
				PackageManager.DONT_KILL_APP);
	}

}
