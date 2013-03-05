package com.rarnu.tools.root.utils;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.MemProcessInfo;

public class ProcessUtils {

	public static List<MemProcessInfo> getUserProcessList() {
		return getProcessList();
	}

	private static List<MemProcessInfo> getProcessList() {

		List<MemProcessInfo> ret = null;

		CommandResult result = RootUtils.runCommand("toolbox ps", false);
		int position = 0;
		if (result != null) {
			if (result.error.equals("")) {
				String r = result.result;
				r = r.toLowerCase();
				// r = r.replaceAll("\\s+", " ");
				String[] ss = r.split("\n");
				ret = new ArrayList<MemProcessInfo>();
				for (int i = 1; i < ss.length; i++) {
					if (!ss[i].startsWith("root")) {
						Log.e("process", ss[i]);
						MemProcessInfo info = null;
						try {
							info = MemProcessInfo.stringToProcessInfo(ss[i]);
						} catch (Exception e) {
							info = null;
						}

						if (info == null) {
							continue;
						}
						if (info.PID > 127) {
							info.position = position;
							ret.add(info);
							position++;
						}
					}
				}
			}
		}
		return ret;
	}
}
