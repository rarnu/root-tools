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
	public static FeedbackFragment fFeedback = null;
	public static IntroFragment fIntro = null;
	public static AboutFragment fAbout = null;
	public static BusyboxFragment fBusybox = null;
	public static SysappFragment fSysapp = null;
	public static SysappDetailFragment fSysappDetail = null;
	public static SysappSelectApkFragment fSysappSelectApk = null;
	public static SettingsFragment fSettings = null;
	public static CleanCacheFragment fCleanCache = null;
	public static EnableappFragment fEnableapp = null;
	public static HtcRomFragment fHtcRom = null;
	public static CompFragment fComp = null;
	public static CompPackageInfoFragment fCompPackageInfo = null;
	public static MemFragment fMem = null;
	public static MemProcessFragment fMemProcess = null;
	public static HostFragment fHost = null;
	public static HostAddFragment fHostAdd = null;
	public static MemIgnoreFragment fMemIgnore = null;
	public static HostEditFragment fHostEdit = null;
	public static HostDeprecatedFragment fHostDeprecated = null;
	public static RecommandFragment fRecommand = null;
	public static DataappReportFragment fDataappReport = null;
	public static BackupFragment fBackup = null;
	public static RestoreFragment fRestore = null;
	public static CustomCleanManagerFragment fCustumClean = null;

	public static void loadFragments() {

		if (fMain == null) {
			fMain = new MainFragment();
		}
		if (fFeedback == null) {
			fFeedback = new FeedbackFragment();
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
		if (fSettings == null) {
			fSettings = new SettingsFragment();
		}
		if (fCleanCache == null) {
			fCleanCache = new CleanCacheFragment();
		}
		if (fEnableapp == null) {
			fEnableapp = new EnableappFragment();
		}
		if (fHtcRom == null) {
			fHtcRom = new HtcRomFragment();
		}
		if (fComp == null) {
			fComp = new CompFragment();
		}
		if (fCompPackageInfo == null) {
			fCompPackageInfo = new CompPackageInfoFragment();
		}
		if (fMem == null) {
			fMem = new MemFragment();
		}
		if (fMemProcess == null) {
			fMemProcess = new MemProcessFragment();
		}
		if (fHost == null) {
			fHost = new HostFragment();
		}
		if (fHostAdd == null) {
			fHostAdd = new HostAddFragment();
		}
		if (fMemIgnore == null) {
			fMemIgnore = new MemIgnoreFragment();
		}
		if (fHostEdit == null) {
			fHostEdit = new HostEditFragment();
		}
		if (fHostDeprecated == null) {
			fHostDeprecated = new HostDeprecatedFragment();
		}
		if (fRecommand == null) {
			fRecommand = new RecommandFragment();
		}
		if (fDataappReport == null) {
			fDataappReport = new DataappReportFragment();
		}
		if (fBackup == null) {
			fBackup = new BackupFragment();
		}
		if (fRestore == null) {
			fRestore = new RestoreFragment();
		}
		if (fCustumClean == null) {
			fCustumClean = new CustomCleanManagerFragment();
		}
	}
	

	public static void releaseFragments() {

		fMain = null;
		fFeedback = null;
		fIntro = null;
		fAbout = null;
		fBusybox = null;
		fSysapp = null;
		fSysappDetail = null;
		fSysappSelectApk = null;
		fSettings = null;
		fCleanCache = null;
		fEnableapp = null;
		fHtcRom = null;
		fComp = null;
		fCompPackageInfo = null;
		fMem = null;
		fHost = null;
		fHostAdd = null;
		fMemIgnore = null;
		fHostEdit = null;
		fHostDeprecated = null;
		fRecommand = null;
		fDataappReport = null;
		fBackup = null;
		fRestore = null;
		fCustumClean = null;
	}

	public static void showContent(Activity activity, Intent inContent,
			Fragment fContent) {
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
