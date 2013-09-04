package com.rarnu.tools.root.common;

public class DiskInfo {

	public String fileSystem;
	public String size;
	public String used;
	public String free;
	public String blockSize;

	public static DiskInfo fromString(String str) {
		DiskInfo di = new DiskInfo();
		String[] items = str.split(" ");
		di.fileSystem = items[0];
		di.size = items[1];
		di.used = items[2];
		di.free = items[3];
		di.blockSize = items[4];
		return di;
	}

	@Override
	public String toString() {
		String ret = String.format(
				"fs:%s, size:%s, used:%s, free:%s, block:%s", fileSystem, size,
				used, free, blockSize);
		return ret;
	}
}
