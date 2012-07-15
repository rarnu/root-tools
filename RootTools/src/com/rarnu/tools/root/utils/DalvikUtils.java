package com.rarnu.tools.root.utils;

import java.io.File;

import com.rarnu.tools.root.utils.root.CommandResult;
import com.rarnu.tools.root.utils.root.RootUtils;

public class DalvikUtils {

	// return -1 for failed
	// return 0 and more for cleaned count
	public static int cleanDalvik() {

		CommandResult cmdResult = RootUtils.runCommand("ls /data/dalvik-cache/", true);
		if (!cmdResult.error.equals("")) {
			return -1;
		}

		String str = cmdResult.result.replace("@", "/").replace("/classes.dex", "");
		String[] dalvikStr = str.split("\n");

		File fClean = null;
		int cleanCount = 0;
		String dalvikName = "";
		CommandResult cleanResult = null;
		if (dalvikStr != null && dalvikStr.length != 0) {
			for (String s : dalvikStr) {
				fClean = new File(s);
				if (!fClean.exists()) {
					dalvikName = s.replace("/", "@");
					cleanResult = RootUtils.runCommand(String.format("rm -r /data/dalvik-cache/%s*", dalvikName), true);
					if (!cleanResult.error.equals("")) {
						return -1;
					} else {
						cleanCount++;
					}
					
				}
			}
		}
		return cleanCount;
	}
}
