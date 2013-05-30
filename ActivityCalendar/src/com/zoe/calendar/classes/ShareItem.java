package com.zoe.calendar.classes;

public class ShareItem {

	public int icon;
	public String title;
	public String text;

	public String packageName;
	public String className;

	public ShareItem(int icon, String title, String text, String packageName, String className) {
		this.icon = icon;
		this.title = title;
		this.text = text;
		this.packageName = packageName;
		this.className = className;
	}

}
