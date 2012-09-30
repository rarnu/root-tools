package com.rarnu.onexcore;

import com.rarnu.tools.root.utils.root.CommandResult;
import com.rarnu.tools.root.utils.root.RootUtils;

public class OneXCoreUtils {

	private static final String TEGRA_CORE_PATH = "/sys/kernel/debug/tegra_hotplug/";
	private static final String TEGRA_MAX_CPU = "max_cpus";
	// private static final String TEGRA_MIN_CPU = "min_cpus";
	// private static final String TEGRA_STATS = "stats";

	public static int getCoreCount() throws Exception {
		String cmd = "cat " + TEGRA_CORE_PATH + TEGRA_MAX_CPU;
		CommandResult cmdRet = RootUtils.runCommand(cmd, true);
		if (cmdRet != null) {
			if (cmdRet.error.equals("")) {
				return Integer.parseInt(cmdRet.result);
			}
		}
		return 0;
	}

	public static boolean setCoreCount(int count) {
		String cmd = "echo " + String.valueOf(count) + " > " + TEGRA_CORE_PATH
				+ TEGRA_MAX_CPU;
		boolean ret = false;
		CommandResult cmdRet = RootUtils.runCommand(cmd, true);
		if (cmdRet != null) {
			if (cmdRet.error.equals("")) {
				ret = true;
			}
		}
		return ret;
	}
}
