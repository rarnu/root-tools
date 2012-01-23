package com.snda.root;

import android.content.ComponentName;
import android.content.pm.PackageManager;

public class ComponentUtils {

	public static boolean doEnabledReceiver(ComponentName receiverName) {

		// pm enable
		// com.melodis.midomiMusicIdentifier/com.google.ads.InstallReceiver
		RootUtils.runRootCommand(String.format("pm enable %s/%s", receiverName
				.getPackageName(), receiverName.getClassName()));

		return GlobalInstance.pm.getComponentEnabledSetting(receiverName) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;

	}

	public static boolean doDisableReceiver(ComponentName receiverName) {

		// pm disable
		// com.melodis.midomiMusicIdentifier/com.google.ads.InstallReceiver
		RootUtils.runRootCommand(String.format("pm disable %s/%s", receiverName
				.getPackageName(), receiverName.getClassName()));
		return GlobalInstance.pm.getComponentEnabledSetting(receiverName) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
	}

}
