package com.snda.root.memory.utils;

import java.util.ArrayList;
import java.util.List;

import com.snda.root.memory.root.CommandResult;
import com.snda.root.memory.root.RootUtils;

public class ProcessUtils {

	public static List<ProcessInfo> getUserProcessList() {
		return getProcessList();
	}

	private static List<ProcessInfo> getProcessList() {

		List<ProcessInfo> ret = null;

		CommandResult result = RootUtils.runRootCommand("ps");
		if (result != null) {
			if (result.error.equals("")) {
				String r = result.result;
				r = r.toLowerCase();
				// r = r.replaceAll("\\s+", " ");
				String[] ss = r.split("\n");
				ret = new ArrayList<ProcessInfo>();
				for (int i = 1; i < ss.length; i++) {
					if (!ss[i].startsWith("root")) {
						ProcessInfo info = ProcessInfo
								.stringToProcessInfo(ss[i]);
						if (info.PPID > 127) {
							ret.add(info);
						}
					}
				}
			}
		}
		return ret;
	}
}
