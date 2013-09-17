package com.rarnu.tools.root;

import android.app.Fragment;

import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.fragment.AboutFragment;
import com.rarnu.tools.root.fragment.BackupFragment;
import com.rarnu.tools.root.fragment.BusyboxFragment;
import com.rarnu.tools.root.fragment.CleanCacheFragment;
import com.rarnu.tools.root.fragment.CompFragment;
import com.rarnu.tools.root.fragment.CompPackageInfoFragment;
import com.rarnu.tools.root.fragment.CustomCleanManagerFragment;
import com.rarnu.tools.root.fragment.DataappReportFragment;
import com.rarnu.tools.root.fragment.DiskInfoFragment;
import com.rarnu.tools.root.fragment.EnableappFragment;
import com.rarnu.tools.root.fragment.FeedbackFragment;
import com.rarnu.tools.root.fragment.FileSystemFragment;
import com.rarnu.tools.root.fragment.HardUpdateFragment;
import com.rarnu.tools.root.fragment.HostAddFragment;
import com.rarnu.tools.root.fragment.HostDeprecatedFragment;
import com.rarnu.tools.root.fragment.HostEditFragment;
import com.rarnu.tools.root.fragment.HostFragment;
import com.rarnu.tools.root.fragment.HtcRomFragment;
import com.rarnu.tools.root.fragment.IndexFragment;
import com.rarnu.tools.root.fragment.MemFragment;
import com.rarnu.tools.root.fragment.MemIgnoreFragment;
import com.rarnu.tools.root.fragment.MemProcessFragment;
import com.rarnu.tools.root.fragment.RecommandFragment;
import com.rarnu.tools.root.fragment.RestoreFragment;
import com.rarnu.tools.root.fragment.SettingsFragment;
import com.rarnu.tools.root.fragment.SysappDetailFragment;
import com.rarnu.tools.root.fragment.SysappFragment;
import com.rarnu.tools.root.fragment.SysappSelectApkFragment;
import com.rarnu.tools.root.fragment.TerminalFragment;

public class Fragments {

	private static IndexFragment fIndex = null;
	private static FeedbackFragment fFeedback = null;
	private static AboutFragment fAbout = null;
	private static BusyboxFragment fBusybox = null;
	private static SysappFragment fSysapp = null;
	private static SysappDetailFragment fSysappDetail = null;
	private static SysappSelectApkFragment fSysappSelectApk = null;
	private static SettingsFragment fSettings = null;
	private static CleanCacheFragment fCleanCache = null;
	private static EnableappFragment fEnableapp = null;
	private static HtcRomFragment fHtcRom = null;
	private static CompFragment fComp = null;
	private static CompPackageInfoFragment fCompPackageInfo = null;
	private static MemFragment fMem = null;
	private static MemProcessFragment fMemProcess = null;
	private static HostFragment fHost = null;
	private static HostAddFragment fHostAdd = null;
	private static MemIgnoreFragment fMemIgnore = null;
	private static HostEditFragment fHostEdit = null;
	private static HostDeprecatedFragment fHostDeprecated = null;
	private static RecommandFragment fRecommand = null;
	private static DataappReportFragment fDataappReport = null;
	private static BackupFragment fBackup = null;
	private static RestoreFragment fRestore = null;
	private static CustomCleanManagerFragment fCustomClean = null;
	private static TerminalFragment fTerminal = null;
	private static HardUpdateFragment fHardUpdate = null;
	private static DiskInfoFragment fDiskInfo = null;
	private static FileSystemFragment fFileSystem = null;

	public static Fragment getFragment(String name) {
		Fragment f = null;
		if (name.equals(FragmentNameConst.FN_INDEX)) {
			if (fIndex == null) {
				fIndex = new IndexFragment();
			}
			f = fIndex;
		} else if (name.equals(FragmentNameConst.FN_FEEDBACK)) {
			if (fFeedback == null) {
				fFeedback = new FeedbackFragment();
			}
			f = fFeedback;
		} else if (name.equals(FragmentNameConst.FN_ABOUT)) {
			if (fAbout == null) {
				fAbout = new AboutFragment();
			}
			f = fAbout;
		} else if (name.equals(FragmentNameConst.FN_BUSYBOX)) {
			if (fBusybox == null) {
				fBusybox = new BusyboxFragment();
			}
			f = fBusybox;
		} else if (name.equals(FragmentNameConst.FN_SYSAPP)) {
			if (fSysapp == null) {
				fSysapp = new SysappFragment();
			}
			f = fSysapp;
		} else if (name.equals(FragmentNameConst.FN_SYSAPP_DETAIL)) {
			if (fSysappDetail == null) {
				fSysappDetail = new SysappDetailFragment();
			}
			f = fSysappDetail;
		} else if (name.equals(FragmentNameConst.FN_SYSAPP_SELECTAPK)) {
			if (fSysappSelectApk == null) {
				fSysappSelectApk = new SysappSelectApkFragment();
			}
			f = fSysappSelectApk;
		} else if (name.equals(FragmentNameConst.FN_SETTINGS)) {
			if (fSettings == null) {
				fSettings = new SettingsFragment();
			}
			f = fSettings;
		} else if (name.equals(FragmentNameConst.FN_CLEAN_CACHE)) {
			if (fCleanCache == null) {
				fCleanCache = new CleanCacheFragment();
			}
			f = fCleanCache;
		} else if (name.equals(FragmentNameConst.FN_ENABLEAPP)) {
			if (fEnableapp == null) {
				fEnableapp = new EnableappFragment();
			}
			f = fEnableapp;
		} else if (name.equals(FragmentNameConst.FN_HTCROM)) {
			if (fHtcRom == null) {
				fHtcRom = new HtcRomFragment();
			}
			f = fHtcRom;
		} else if (name.equals(FragmentNameConst.FN_COMP)) {
			if (fComp == null) {
				fComp = new CompFragment();
			}
			f = fComp;
		} else if (name.equals(FragmentNameConst.FN_COMP_PACKAGE_INFO)) {
			if (fCompPackageInfo == null) {
				fCompPackageInfo = new CompPackageInfoFragment();
			}
			f = fCompPackageInfo;
		} else if (name.equals(FragmentNameConst.FN_MEM)) {
			if (fMem == null) {
				fMem = new MemFragment();
			}
			f = fMem;
		} else if (name.equals(FragmentNameConst.FN_MEM_PROCESS)) {
			if (fMemProcess == null) {
				fMemProcess = new MemProcessFragment();
			}
			f = fMemProcess;
		} else if (name.equals(FragmentNameConst.FN_HOST)) {
			if (fHost == null) {
				fHost = new HostFragment();
			}
			f = fHost;
		} else if (name.equals(FragmentNameConst.FN_HOST_ADD)) {
			if (fHostAdd == null) {
				fHostAdd = new HostAddFragment();
			}
			f = fHostAdd;
		} else if (name.equals(FragmentNameConst.FN_MEM_IGNORE)) {
			if (fMemIgnore == null) {
				fMemIgnore = new MemIgnoreFragment();
			}
			f = fMemIgnore;
		} else if (name.equals(FragmentNameConst.FN_HOST_EDIT)) {
			if (fHostEdit == null) {
				fHostEdit = new HostEditFragment();
			}
			f = fHostEdit;
		} else if (name.equals(FragmentNameConst.FN_HOST_DEPRECATED)) {
			if (fHostDeprecated == null) {
				fHostDeprecated = new HostDeprecatedFragment();
			}
			f = fHostDeprecated;
		} else if (name.equals(FragmentNameConst.FN_RECOMMAND)) {
			if (fRecommand == null) {
				fRecommand = new RecommandFragment();
			}
			f = fRecommand;
		} else if (name.equals(FragmentNameConst.FN_DATAAPP_REPORT)) {
			if (fDataappReport == null) {
				fDataappReport = new DataappReportFragment();
			}
			f = fDataappReport;
		} else if (name.equals(FragmentNameConst.FN_BACKUP)) {
			if (fBackup == null) {
				fBackup = new BackupFragment();
			}
			f = fBackup;
		} else if (name.equals(FragmentNameConst.FN_RESTORE)) {
			if (fRestore == null) {
				fRestore = new RestoreFragment();
			}
			f = fRestore;
		} else if (name.equals(FragmentNameConst.FN_CUSTOM_CLEAN)) {
			if (fCustomClean == null) {
				fCustomClean = new CustomCleanManagerFragment();
			}
			f = fCustomClean;
		} else if (name.equals(FragmentNameConst.FN_TERMINAL)) {
			if (fTerminal == null) {
				fTerminal = new TerminalFragment();
			}
			f = fTerminal;
		} else if (name.equals(FragmentNameConst.FN_HARD_UPDATE)) {
			if (fHardUpdate == null) {
				fHardUpdate = new HardUpdateFragment();
			}
			f = fHardUpdate;
		} else if (name.equals(FragmentNameConst.FN_DISKINFO)) {
			if (fDiskInfo == null) {
				fDiskInfo = new DiskInfoFragment();
			}
			f = fDiskInfo;
		} else if (name.equals(FragmentNameConst.FN_FILESYSTEM)) {
			if (fFileSystem == null) {
				fFileSystem = new FileSystemFragment();
			}
			f = fFileSystem;
		}
		return f;
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
		fCustomClean = null;
		fTerminal = null;
		fHardUpdate = null;
		fDiskInfo = null;
		fFileSystem = null;
	}
}
