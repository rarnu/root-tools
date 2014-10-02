package com.yugioh.android.classes;

public class RightMenuItem {

	public String name;
	/**
	 * 0:update
	 * 1:fitable
	 */
	public int type = 0;
	public int value = 0;

    public RightMenuItem(String name) {
        this.name = name;
    }
}
