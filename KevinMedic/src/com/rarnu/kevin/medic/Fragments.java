package com.rarnu.kevin.medic;

import com.rarnu.kevin.medic.fragment.LeftMenuFragment;
import com.rarnu.kevin.medic.fragment.MainFragment;
import com.rarnu.kevin.medic.fragment.Page1Fragment;
import com.rarnu.kevin.medic.fragment.RightMenuFragment;

public class Fragments {

	public static MainFragment fMain = null;
	public static LeftMenuFragment fLeftMenu = null;
	public static RightMenuFragment fRightMenu = null;
	public static Page1Fragment fPage1 = null;
	
	
	public static void load() {
		fMain = new MainFragment();
		fLeftMenu = new LeftMenuFragment();
		fRightMenu = new RightMenuFragment();
		fPage1 = new Page1Fragment();
	}
	
	public static void release() {
		fMain = null;
		fLeftMenu = null;
		fRightMenu = null;
		fPage1 = null;
	}
}
