package com.rarnu.kevin.medic;

import com.rarnu.kevin.medic.fragment.LeftMenuFragment;
import com.rarnu.kevin.medic.fragment.MainFragment;
import com.rarnu.kevin.medic.fragment.Page1Fragment;
import com.rarnu.kevin.medic.fragment.Page2Fragment;
import com.rarnu.kevin.medic.fragment.Page3Fragment;
import com.rarnu.kevin.medic.fragment.Page4Fragment;
import com.rarnu.kevin.medic.fragment.Page5Fragment;
import com.rarnu.kevin.medic.fragment.RightMenuFragment;

public class Fragments {

	public static MainFragment fMain = null;
	public static LeftMenuFragment fLeftMenu = null;
	public static RightMenuFragment fRightMenu = null;
	public static Page1Fragment fPage1 = null;
	public static Page2Fragment fPage2 = null;
	public static Page3Fragment fPage3 = null;
	public static Page4Fragment fPage4 = null;
	public static Page5Fragment fPage5 = null;
	
	
	public static void load() {
		fMain = new MainFragment();
		fLeftMenu = new LeftMenuFragment();
		fRightMenu = new RightMenuFragment();
		fPage1 = new Page1Fragment();
		fPage2 = new Page2Fragment();
		fPage3 = new Page3Fragment();
		fPage4 = new Page4Fragment();
		fPage5 = new Page5Fragment();
	}
	
	public static void release() {
		fMain = null;
		fLeftMenu = null;
		fRightMenu = null;
		fPage1 = null;
		fPage2 = null;
		fPage3 = null;
		fPage4 = null;
		fPage5 = null;
	}
}
