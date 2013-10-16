package com.rarnu.tools.root;

import android.app.Fragment;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.fragment.*;

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
    private static PoolFragment fPool = null;
    private static TextEditorFragment fTextEditor = null;

    public static Fragment getFragment(String name) {
        releaseOtherFragment(name);
        Fragment f = null;
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_INDEX, IndexFragment.class, fIndex);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_FEEDBACK, FeedbackFragment.class, fFeedback);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_ABOUT, AboutFragment.class, fAbout);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_BUSYBOX, BusyboxFragment.class, fBusybox);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_SYSAPP, SysappFragment.class, fSysapp);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_SYSAPP_DETAIL, SysappDetailFragment.class, fSysappDetail);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_SYSAPP_SELECTAPK, SysappSelectApkFragment.class, fSysappSelectApk);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_SETTINGS, SettingsFragment.class, fSettings);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_CLEAN_CACHE, CleanCacheFragment.class, fCleanCache);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_ENABLEAPP, EnableappFragment.class, fEnableapp);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_HTCROM, HtcRomFragment.class, fHtcRom);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_COMP, CompFragment.class, fComp);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_COMP_PACKAGE_INFO, CompPackageInfoFragment.class, fCompPackageInfo);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_MEM, MemFragment.class, fMem);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_MEM_PROCESS, MemProcessFragment.class, fMemProcess);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_HOST, HostFragment.class, fHost);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_HOST_ADD, HostAddFragment.class, fHostAdd);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_MEM_IGNORE, MemIgnoreFragment.class, fMemIgnore);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_HOST_EDIT, HostEditFragment.class, fHostEdit);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_HOST_DEPRECATED, HostDeprecatedFragment.class, fHostDeprecated);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_RECOMMAND, RecommandFragment.class, fRecommand);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_DATAAPP_REPORT, DataappReportFragment.class, fDataappReport);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_BACKUP, BackupFragment.class, fBackup);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_RESTORE, RestoreFragment.class, fRestore);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_CUSTOM_CLEAN, CustomCleanManagerFragment.class, fCustomClean);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_TERMINAL, TerminalFragment.class, fTerminal);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_HARD_UPDATE, HardUpdateFragment.class, fHardUpdate);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_DISKINFO, DiskInfoFragment.class, fDiskInfo);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_FILESYSTEM, FileSystemFragment.class, fFileSystem);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_POOL, PoolFragment.class, fPool);
        }
        if (f == null) {
            f = getFragment(name, FragmentNameConst.FN_TEXT_EDITOR, TextEditorFragment.class, fTextEditor);
        }

        return f;
    }

    private static Fragment getFragment(String name, String checkName, Class<?> fClass, Fragment f) {
        if (name.equals(checkName)) {
            if (f == null) {
                try {
                    f = (Fragment) fClass.newInstance();
                } catch (Exception e) {

                }
            }
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
        fPool = null;
        fTextEditor = null;
    }

    public static void releaseOtherFragment(String name) {
        doReleaseFragment(name, FragmentNameConst.FN_FEEDBACK, fFeedback);
        doReleaseFragment(name, FragmentNameConst.FN_ABOUT, fAbout);
        doReleaseFragment(name, FragmentNameConst.FN_BUSYBOX, fBusybox);
        doReleaseFragment(name, FragmentNameConst.FN_SYSAPP, fSysapp);
        doReleaseFragment(name, FragmentNameConst.FN_SYSAPP_DETAIL, fSysappDetail);
        doReleaseFragment(name, FragmentNameConst.FN_SYSAPP_SELECTAPK, fSysappSelectApk);
        doReleaseFragment(name, FragmentNameConst.FN_SETTINGS, fSettings);
        doReleaseFragment(name, FragmentNameConst.FN_CLEAN_CACHE, fCleanCache);
        doReleaseFragment(name, FragmentNameConst.FN_ENABLEAPP, fEnableapp);
        doReleaseFragment(name, FragmentNameConst.FN_HTCROM, fHtcRom);
        doReleaseFragment(name, FragmentNameConst.FN_COMP, fComp);
        doReleaseFragment(name, FragmentNameConst.FN_COMP_PACKAGE_INFO, fCompPackageInfo);
        doReleaseFragment(name, FragmentNameConst.FN_MEM, fMem);
        doReleaseFragment(name, FragmentNameConst.FN_HOST, fHost);
        doReleaseFragment(name, FragmentNameConst.FN_HOST_ADD, fHostAdd);
        doReleaseFragment(name, FragmentNameConst.FN_MEM_IGNORE, fMemIgnore);
        doReleaseFragment(name, FragmentNameConst.FN_HOST_EDIT, fHostEdit);
        doReleaseFragment(name, FragmentNameConst.FN_HOST_DEPRECATED, fHostDeprecated);
        doReleaseFragment(name, FragmentNameConst.FN_RECOMMAND, fRecommand);
        doReleaseFragment(name, FragmentNameConst.FN_DATAAPP_REPORT, fDataappReport);
        doReleaseFragment(name, FragmentNameConst.FN_BACKUP, fBackup);
        doReleaseFragment(name, FragmentNameConst.FN_RESTORE, fRestore);
        doReleaseFragment(name, FragmentNameConst.FN_CUSTOM_CLEAN, fCustomClean);
        doReleaseFragment(name, FragmentNameConst.FN_TERMINAL, fTerminal);
        doReleaseFragment(name, FragmentNameConst.FN_HARD_UPDATE, fHardUpdate);
        doReleaseFragment(name, FragmentNameConst.FN_DISKINFO, fDiskInfo);
        doReleaseFragment(name, FragmentNameConst.FN_FILESYSTEM, fFileSystem);
        doReleaseFragment(name, FragmentNameConst.FN_POOL, fPool);
        doReleaseFragment(name, FragmentNameConst.FN_TEXT_EDITOR, fTextEditor);
    }

    private static void doReleaseFragment(String name, String checkName, Fragment f) {
        if (!name.equals(checkName)) {
            f = null;
        }
    }
}
