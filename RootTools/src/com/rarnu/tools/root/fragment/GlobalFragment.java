package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.content.Intent;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;

public class GlobalFragment {

	public static MainFragment fMain = new MainFragment();
	public static FeedbackFragment fFeedback =new FeedbackFragment();
	public static IntroFragment fIntro = new IntroFragment();
	public static AboutFragment fAbout = new AboutFragment();
	public static BusyboxFragment fBusybox = new BusyboxFragment();
	public static SysappFragment fSysapp = new SysappFragment();
	public static SysappDetailFragment fSysappDetail = new SysappDetailFragment();
	public static SysappSelectApkFragment fSysappSelectApk = new SysappSelectApkFragment();
	
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
