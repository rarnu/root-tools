package com.rarnu.tools.root.fragment;

public class GlobalFragment {

	public static IndexFragment fIndex = null;
	public static FeedbackFragment fFeedback = null;
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
		fIndex = new IndexFragment();
		fFeedback = new FeedbackFragment();
		fAbout = new AboutFragment();
		fBusybox = new BusyboxFragment();
		fSysapp = new SysappFragment();
		fSysappDetail = new SysappDetailFragment();
		fSysappSelectApk = new SysappSelectApkFragment();
		fSettings = new SettingsFragment();
		fCleanCache = new CleanCacheFragment();
		fEnableapp = new EnableappFragment();
		fHtcRom = new HtcRomFragment();
		fComp = new CompFragment();
		fCompPackageInfo = new CompPackageInfoFragment();
		fMem = new MemFragment();
		fMemProcess = new MemProcessFragment();
		fHost = new HostFragment();
		fHostAdd = new HostAddFragment();
		fMemIgnore = new MemIgnoreFragment();
		fHostEdit = new HostEditFragment();
		fHostDeprecated = new HostDeprecatedFragment();
		fRecommand = new RecommandFragment();
		fDataappReport = new DataappReportFragment();
		fBackup = new BackupFragment();
		fRestore = new RestoreFragment();
		fCustumClean = new CustomCleanManagerFragment();
	}

	public static void releaseFragments() {

		fIndex = null;
		fFeedback = null;
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
}
