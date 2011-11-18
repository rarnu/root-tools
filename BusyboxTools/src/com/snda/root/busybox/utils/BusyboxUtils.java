package com.snda.root.busybox.utils;

import java.util.ArrayList;
import java.util.List;

import com.snda.root.busybox.adapter.BusyboxItem;

public class BusyboxUtils {

	public static List<BusyboxItem> getBusyboxItems() {
		List<BusyboxItem> list = null;
		CommandResult result = RootUtils.runRootCommand("busybox --list");
		if (result.error.equals("")) {
			String str = result.result.trim();
			String[] items = str.split("\n");
			if (items != null) {
				if (items.length > 0) {
					list = new ArrayList<BusyboxItem>();
					for (String itm : items) {
						BusyboxItem item = new BusyboxItem();
						item.name = itm;
						list.add(item);
					}
				}
			}
		}

		return list;
	}

	public static String getCommandHelp(String cmd) {
		String help = "";
		CommandResult result = RootUtils.runRootCommand("busybox " + cmd
				+ " --help");
		if (result != null) {
			help = result.error.trim();
		}
		return help;
	}
}
