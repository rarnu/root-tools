package com.rarnu.tools.root.utils;

import java.util.ArrayList;
import java.util.List;

import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.utils.root.CommandResult;
import com.rarnu.tools.root.utils.root.RootUtils;

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
						MemProcessInfo info = MemProcessInfo.stringToProcessInfo(ss[i]);
						if (info == null) {
							continue;
						}
						if (info.PPID > 127) {
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
