package com.rarnu.findaround.common;

public class FixedPageItem {
	
	public String name;
	public int page;
	public int position;

	public static FixedPageItem build(String s) {
		// 1-1|name
		String[] details = s.split("\\|");
		FixedPageItem result = new FixedPageItem();
		result.name = details[1];
		String[] pos = details[0].split("-");
		result.page = Integer.parseInt(pos[0]);
		result.position = Integer.parseInt(pos[1]);
		return result;
	}
}
