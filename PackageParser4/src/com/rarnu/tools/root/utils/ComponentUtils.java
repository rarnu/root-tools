package com.rarnu.tools.root.utils;

import android.app.ActivityManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.util.DisplayMetrics;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.CompInfo;
import com.rarnu.tools.root.common.EnableappInfo;

import java.util.ArrayList;
import java.util.List;

public class ComponentUtils {

    public static boolean doEnableApplication(EnableappInfo info) {
        String source = info.filePath;
        String filePath = "";
        String dest = "";
        if (info.type == 0) {
            dest = "/system/app/";
            filePath = dest + source.replace(DirHelper.ENABLEAPP_DIR_SYSTEM, "");
        } else if (info.type == 1) {
            dest = "/data/app/";
            filePath = dest + source.replace(DirHelper.ENABLEAPP_DIR_DATA, "");
        } else if (info.type == 3) {
            dest = "/system/priv-app/";
            filePath = dest + source.replace(DirHelper.ENABLEAPP_DIR_PRIVATE, "");
        }
        try {
            CommandResult cmdRet = RootUtils.runCommand(String.format("busybox cp %s %s", source, dest), true, null);
            if (cmdRet.error.equals("")) {
                cmdRet = RootUtils.runCommand(String.format("rm %s", source), true, null);
                if (cmdRet.error.equals("")) {
                    cmdRet = RootUtils.runCommand("chmod 644 " + filePath, true, null);
                }
                info.filePath = filePath;
            }
            return cmdRet.error.equals("");
        } catch (Throwable th) {
            return false;
        }
    }

    public static boolean doDisableApplication(EnableappInfo info) {
        String source = info.info.sourceDir;
        String filePath = "";
        String dest = "";
        if (info.type == 0) {
            dest = DirHelper.ENABLEAPP_DIR_SYSTEM;
            filePath = dest + source.replace("/system/app/", "");
        } else if (info.type == 1) {
            dest = DirHelper.ENABLEAPP_DIR_DATA;
            filePath = dest + source.replace("/data/app/", "");
        } else if (info.type == 3) {
            dest = DirHelper.ENABLEAPP_DIR_PRIVATE;
            filePath = dest + source.replace("/system/priv-app/", "");
        } else {
            return false;
        }
        try {
            CommandResult cmdRet = RootUtils.runCommand(String.format("busybox cp %s %s", source, dest), true, null);
            if (cmdRet.error.equals("")) {
                cmdRet = RootUtils.runCommand(String.format("rm %s", source), true, null);
                info.filePath = filePath;
            }
            return cmdRet.error.equals("");
        } catch (Throwable th) {
            return false;
        }
    }

    public static boolean doEnabledComponent(Context context, ComponentName receiverName) {

        try {
            RootUtils.runCommand(String.format("pm enable '%s/%s'", receiverName.getPackageName(), receiverName.getClassName()), true, null);
            return context.getPackageManager().getComponentEnabledSetting(receiverName) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
        } catch (Throwable th) {
            return false;
        }
    }

    public static boolean doDisableComponent(Context context, ComponentName receiverName) {
        try {
            RootUtils.runCommand(String.format("pm disable '%s/%s'", receiverName.getPackageName(), receiverName.getClassName()), true, null);
            return context.getPackageManager().getComponentEnabledSetting(receiverName) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
        } catch (Throwable th) {
            return false;
        }
    }

    public static Object /* PackageParser.Package */ parsePackageInfo(PackageInfo info) {
        String fileAbsPath = info.applicationInfo.publicSourceDir;
        PackageParserUtils ppu = new PackageParserUtils(fileAbsPath);
        Object pkg = ppu.parsePackage(fileAbsPath, PackageParserUtils.PARSE_IS_SYSTEM);
        return pkg;
    }

    public static List<CompInfo> getPackageRSList(Context context, Object /* PackageParser.Package */ pkg) {
        List<CompInfo> lstComponentInfo = new ArrayList<CompInfo>();
        if (context != null) {
            PackageManager pm = context.getPackageManager();
            // List<PackageParser.Activity> lstReceiver = pkg.receivers;
            ArrayList<Object> lstReceiver = PackageParserUtils.packageReceivers(pkg);
            for (Object /* PackageParser.Activity */ a : lstReceiver) {
                PackageParserUtils.Activity aa = PackageParserUtils.Activity.fromComponent(a);
                CompInfo info = new CompInfo();
                info.component = aa;
                info.fullPackageName = aa.getComponentName().getClassName();
                info.enabled = pm.getComponentEnabledSetting(aa.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
                lstComponentInfo.add(info);
            }

            // List<PackageParser.Service> lstService = pkg.services;
            ArrayList<Object> lstService = PackageParserUtils.packageServices(pkg);
            for (Object /* PackageParser.Service */ s : lstService) {
                PackageParserUtils.Service ss = PackageParserUtils.Service.fromComponent(s);
                CompInfo info = new CompInfo();
                info.component = ss;
                info.fullPackageName = ss.getComponentName().getClassName();
                info.enabled = pm.getComponentEnabledSetting(ss.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
                lstComponentInfo.add(info);
            }
        }
        return lstComponentInfo;
    }

    public static ComponentName getPackageComponentName(Object /* PackageParser.Component<?> */ comp) {
        return ((PackageParserUtils.Component) comp).getComponentName();
    }

    public static boolean isServiceRunning(Context context, String className) {
        boolean isRunning = false;
        ActivityManager activityManager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        List<ActivityManager.RunningServiceInfo> serviceList = activityManager.getRunningServices(30);
        if (!(serviceList.size() > 0)) {
            return false;
        }
        for (ActivityManager.RunningServiceInfo si : serviceList) {
            if (si.service.getClassName().equals(className) == true) {
                isRunning = true;
                break;
            }
        }
        return isRunning;
    }
}
