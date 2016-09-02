package com.rarnu.tools.neo.utils;

import android.content.Context;
import android.content.pm.PackageManager;
import com.rarnu.tools.neo.data.CompInfo;
import com.rarnu.tools.neo.root.CommandResult;
import com.rarnu.tools.neo.root.RootUtils;

import java.util.ArrayList;
import java.util.List;

@SuppressWarnings("Duplicates")
public class ComponentUtils {

    public static boolean applicationFreeze(String pkgName, boolean isDisable) {
        String cmd = String.format("pm %s %s", (isDisable ? "disable" : "enable"), pkgName);
        CommandResult result = RootUtils.runCommand(cmd, true);
        return result.error.equals("");
    }

    public static boolean componentFreeze(String pkgName, String componentName, boolean isDisable) {
        String cmd = String.format("pm %s %s/%s", (isDisable ? "disable" : "enable"), pkgName, componentName);
        CommandResult result = RootUtils.runCommand(cmd, true);
        return result.error.equals("");
    }

    public static List<CompInfo> getActivityList(Context ctx, Object pkg) {
        List<CompInfo> lstComponentInfo = new ArrayList<>();
        if (ctx != null) {
            PackageManager pm = ctx.getPackageManager();
            ArrayList<Object> lst = PackageParserUtils.packageActivities(pkg);
            if (lst != null) {
                for (Object a: lst) {
                    PackageParserUtils.Activity aa = PackageParserUtils.Activity.fromComponent(a);
                    CompInfo info = new CompInfo();
                    info.component = aa;
                    info.fullPackageName = aa.getComponentName().getClassName();
                    info.enabled = pm.getComponentEnabledSetting(aa.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
                    lstComponentInfo.add(info);
                }
            }
        }
        return lstComponentInfo;
    }

    public static List<CompInfo> getServiceList(Context ctx, Object pkg) {
        List<CompInfo> lstComponentInfo = new ArrayList<>();
        if (ctx != null) {
            PackageManager pm = ctx.getPackageManager();
            ArrayList<Object> lst = PackageParserUtils.packageServices(pkg);
            if (lst != null) {
                for (Object a: lst) {
                    PackageParserUtils.Service aa = PackageParserUtils.Service.fromComponent(a);
                    CompInfo info = new CompInfo();
                    info.component = aa;
                    info.fullPackageName = aa.getComponentName().getClassName();
                    info.enabled = pm.getComponentEnabledSetting(aa.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
                    lstComponentInfo.add(info);
                }
            }
        }
        return lstComponentInfo;
    }

    public static List<CompInfo> getReceiverList(Context ctx, Object pkg) {
        List<CompInfo> lstComponentInfo = new ArrayList<>();
        if (ctx != null) {
            PackageManager pm = ctx.getPackageManager();
            ArrayList<Object> lst = PackageParserUtils.packageReceivers(pkg);
            if (lst != null) {
                for (Object a: lst) {
                    PackageParserUtils.Activity aa = PackageParserUtils.Activity.fromComponent(a);
                    CompInfo info = new CompInfo();
                    info.component = aa;
                    info.fullPackageName = aa.getComponentName().getClassName();
                    info.enabled = pm.getComponentEnabledSetting(aa.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
                    lstComponentInfo.add(info);
                }
            }
        }
        return lstComponentInfo;
    }

    public static List<CompInfo> getProviderList(Context ctx, Object pkg) {
        List<CompInfo> lstComponentInfo = new ArrayList<>();
        if (ctx != null) {
            PackageManager pm = ctx.getPackageManager();
            ArrayList<Object> lst = PackageParserUtils.packageProviders(pkg);
            if (lst != null) {
                for (Object a: lst) {
                    PackageParserUtils.Provider aa = PackageParserUtils.Provider.fromComponent(a);
                    CompInfo info = new CompInfo();
                    info.component = aa;
                    info.fullPackageName = aa.getComponentName().getClassName();
                    info.enabled = pm.getComponentEnabledSetting(aa.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
                    lstComponentInfo.add(info);
                }
            }
        }
        return lstComponentInfo;
    }

}
