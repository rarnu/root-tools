package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.content.Intent;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;

public class GlobalFragment {

	public static MainFragment fMain = null;
	public static FeedbackFragment fFeedback =null;
	public static IntroFragment fIntro = null;
	public static AboutFragment fAbout = null;
	public static BusyboxFragment fBusybox = null;
	public static SysappFragment fSysapp = null;
	public static SysappDetailFragment fSysappDetail = null;
	public static SysappSelectApkFragment fSysappSelectApk = null;
	
	public static void loadFragments() {
		if (fMain == null) {
			fMain = new MainFragment();
		}
		if (fFeedback == null) {
			fFeedback =new FeedbackFragment();
		}
		
		if (fIntro == null) {
			fIntro = new IntroFragment();
		}
		if (fAbout == null) {
			fAbout = new AboutFragment();
		}
		if (fBusybox == null) {
			fBusybox = new BusyboxFragment();
		}
		if (fSysapp == null) {
			fSysapp = new SysappFragment();
		}
		if (fSysappDetail == null) {
			fSysappDetail = new SysappDetailFragment();
		}
		if (fSysappSelectApk == null) {
			fSysappSelectApk = new SysappSelectApkFragment();
		}
	}
	
	public static void showContent(Activity activity, Intent inContent, Fragment fContent) {
		if (GlobalInstance.dualPane) {
			FragmentManager fragmentManager = activity.getFragmentManager();
			FragmentTransaction fragmentTransaction = fragmentManager
					.beginTransaction();
			fragmentTransaction.replace(R.id.fragmentDetail, fContent);
			fragmentTransaction
					.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE);
			fragmentTransaction.commit();
		} else {
			activity.startActivity(inContent);
		}
	}
}
