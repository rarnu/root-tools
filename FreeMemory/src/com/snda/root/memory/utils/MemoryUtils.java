package com.snda.root.memory.utils;

import com.snda.root.memory.root.CommandResult;
import com.snda.root.memory.root.RootUtils;

public class MemoryUtils {

	public static MemoryInfo getMemoryInfo() {

		MemoryInfo ret = null;
		CommandResult result = RootUtils.runRootCommand("busybox free");
		if (result != null) {
			if (result.error.equals("")) {
				ret = new MemoryInfo();
				
				String r = result.result;
				r = r.toLowerCase();
				r = r.replace("total", "").replace("used", "").replace("free",
						"").replace("shared", "").replace("buffers", "").replace("mem:", "");
				r = r.replaceAll("\\s+", " ").trim();
				
				String[] ss = r.split(" ");
				ret.Total = Integer.parseInt(ss[0]) / 1024;
				ret.Used = Integer.parseInt(ss[1]) / 1024;
				ret.Free = Integer.parseInt(ss[2]) /1024;
				ret.Shared = Integer.parseInt(ss[3]) / 1024;
				ret.Buffer = Integer.parseInt(ss[4]) / 1024;

			}
		}

		return ret;
	}

}
