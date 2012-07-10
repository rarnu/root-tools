package com.rarnu.tools.root.utils;

import java.io.File;

import android.content.ComponentName;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageParser;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.utils.root.RootUtils;

public class ComponentUtils {

	public static boolean doEnabledReceiver(ComponentName receiverName) {

		// pm enable
		// com.melodis.midomiMusicIdentifier/com.google.ads.InstallReceiver
		try {
			RootUtils.runCommand(
					String.format("pm enable %s/%s", receiverName.getPackageName(), receiverName.getClassName()), true);

			return GlobalInstance.pm.getComponentEnabledSetting(receiverName) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
		} catch (Throwable th) {
			return false;
		}
	}

	public static boolean doDisableReceiver(ComponentName receiverName) {
		// pm disable
		// com.melodis.midomiMusicIdentifier/com.google.ads.InstallReceiver
		try {
			RootUtils
					.runCommand(
							String.format("pm disable %s/%s", receiverName.getPackageName(),
									receiverName.getClassName()), true);
			return GlobalInstance.pm.getComponentEnabledSetting(receiverName) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
		} catch (Throwable th) {
			return false;
		}
	}

	public static PackageParser.Package parsePackageInfo(PackageInfo info) {
		String fileAbsPath = info.applicationInfo.publicSourceDir;
		PackageParser packageParser = new PackageParser(fileAbsPath);
		File sourceFile = new File(fileAbsPath);
		PackageParser.Package pkg = packageParser.parsePackage(sourceFile, fileAbsPath, GlobalInstance.metric,
				PackageParser.PARSE_IS_SYSTEM);
		return pkg;
	}
}
