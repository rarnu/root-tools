package com.rarnu.tools.root.common;

import com.rarnu.tools.root.utils.DiskUtils;

public class DiskInfo {

	public String fileSystem;
	public String size;
	public String used;
	public String free;
	public String blockSize;

	public static DiskInfo fromString(String str) {
		DiskInfo di = null;
		String[] items = str.split(" ");
		if (!items[0].contains(".") && !items[0].contains("emulated")) {
			di = new DiskInfo();
			di.fileSystem = items[0];
			di.size = items[1];
			di.used = items[2];
			di.free = items[3];
			di.blockSize = items[4];
		}
		return di;
	}

	/**
	 * 
	 * @param type
	 *            0:used, 1:free
	 * @return html
	 */
	public String getColoredSize(int type) {
		double percent = 0D;
		String colorStr = "";

		switch (type) {
		case 0:
			percent = DiskUtils.stringToKbytes(used)
					/ DiskUtils.stringToKbytes(size);
			if (percent <= 0.6D) {
				colorStr = "#00FF00";
			} else if (percent > 0.6 && percent <= 0.9) {
				colorStr = "#FFFF00";
			} else if (percent > 0.9) {
				colorStr = "#FF0000";
			}
			break;
		case 1:
			percent = DiskUtils.stringToKbytes(free)
					/ DiskUtils.stringToKbytes(size);
			if (percent >= 0.4D) {
				colorStr = "#00FF00";
			} else if (percent < 0.4 && percent >= 0.1) {
				colorStr = "#FFFF00";
			} else if (percent < 0.1) {
				colorStr = "#FF0000";
			}
			break;
		}

		return String.format("<font color=%s>%s</font>", colorStr,
				(type == 0 ? used : free));
	}
}
