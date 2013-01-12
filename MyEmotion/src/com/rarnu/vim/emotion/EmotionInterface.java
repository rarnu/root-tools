package com.rarnu.vim.emotion;

import android.app.Fragment;

public interface EmotionInterface {
	
	public static final int FRAGMENT_CENTER = 0;
	public static final int FRAGMENT_TOP = 1;
	public static final int FRAGMENT_BOTTOM = 2;
	public static final int FRAGMENT_LEFT = 3;
	
	void setCurrentFace(String faceName, String comment);
	void setTopBottomScrollable(boolean enable);
	void setLeftRightScrollable(boolean enable);
	void switchFragment(int layout, Fragment fragment);
	Fragment getFragment(int id);
}
