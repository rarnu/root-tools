package com.rarnu.tools.root.common;

public class BusyboxInfo {

	public static final int STATE_BANNED = 0;
	public static final int STATE_WARNING = 1;
	public static final int STATE_NORMAL = 2;
	
	public String title;
	public int state;

    /**
     * true: hide special icon<br />
     * false: show special icon
     */
    public boolean isAppletsRight;
}
