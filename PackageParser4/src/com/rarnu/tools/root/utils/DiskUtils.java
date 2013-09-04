package com.rarnu.tools.root.utils;

import java.util.ArrayList;
import java.util.List;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.DiskInfo;

public class DiskUtils {

	public static List<DiskInfo> getDiskInfoList() {
		CommandResult cmdRet = RootUtils.runCommand("df", false);
		String[] dis = cmdRet.result.split("\n");
		List<DiskInfo> list = null;
		if (dis != null && dis.length != 0) {
			// skip the first line
			list = new ArrayList<DiskInfo>();
			String strDi = "";
			for (int i = 1; i < dis.length; i++) {
				if (!dis[i].contains(".")) {
					// skip apps installed on sdcard
					strDi = dis[i].replaceAll("\\s+", " ");
					list.add(DiskInfo.fromString(strDi));
				}
			}
		}
		return list;
	}
}
