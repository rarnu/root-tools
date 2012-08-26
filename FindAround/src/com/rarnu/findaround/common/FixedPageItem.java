package com.rarnu.findaround.common;

public class FixedPageItem {
	
	public String name;
	public int page;
	public int position;
	public int image;

	public static FixedPageItem build(String s) {
		// 1-1|name|0
		s = s.trim();
		String[] details = s.split("\\|");
		FixedPageItem result = new FixedPageItem();
		result.name = details[1];
		String[] pos = details[0].split("-");
		result.page = Integer.parseInt(pos[0]);
		result.position = Integer.parseInt(pos[1]);
		result.image = Integer.parseInt(details[2]);
		return result;
	}
}
