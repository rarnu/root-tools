package com.rarnu.tools.root.utils;

import android.Manifest;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.util.DisplayMetrics;
import com.rarnu.tools.root.common.AutobootInfo;

import java.util.ArrayList;
import java.util.List;

public class AutobootUtils {

    public static List<AutobootInfo> getAutobootApps(Context context, DisplayMetrics dm) {
        List<AutobootInfo> list = new ArrayList<AutobootInfo>();
        if (context != null) {
            PackageManager pm = context.getPackageManager();
            List<PackageInfo> apps = null;
            try {
                apps = pm.getInstalledPackages(0);
            } catch (Exception e) {

            }
            int status = -1;
            if (apps != null && apps.size() != 0) {
                for (PackageInfo pi : apps) {
                    if (pm.checkPermission(Manifest.permission.RECEIVE_BOOT_COMPLETED, pi.packageName) == PackageManager.PERMISSION_GRANTED) {
                        status = isEnabled(context, pi);
                        if (status != -1) {
                            AutobootInfo info = new AutobootInfo();
                            info.info = pi;
                            info.enabled = (status != 0);
                            list.add(info);
                        }
                    }
                }
            }
        }
        return list;
    }

    public static boolean switchAutoboot(Context context, AutobootInfo info, boolean enabled) {
        boolean ret = true;
        boolean operatingResult = true;
        Object /* PackageParser.Package */ pkg = ComponentUtils.parsePackageInfo(info.info);
        // List<PackageParser.Activity> receivers = pkg.receivers;
        ArrayList<Object> receivers = PackageParserUtils.packageReceivers(pkg);

        for (Object /* PackageParser.Activity */ riobj : receivers) {
            PackageParserUtils.Activity ri = PackageParserUtils.Activity.fromComponent(riobj);
            if (ri.intents != null && ri.intents.size() != 0) {
                for (Object /* PackageParser.ActivityIntentInfo */ aiiobj : ri.intents) {
                    IntentFilter aii = (IntentFilter) aiiobj;
                    if (aii.countActions() > 0) {
                        for (int i = 0; i < aii.countActions(); i++) {
                            if (aii.getAction(i).equals(Intent.ACTION_BOOT_COMPLETED)) {
                                if (enabled) {
                                    operatingResult = ComponentUtils.doEnabledComponent(context, ri.getComponentName());
                                } else {
                                    operatingResult = ComponentUtils.doDisableComponent(context, ri.getComponentName());
                                }
                                if (!operatingResult) {
                                    ret = false;
                                }
                                break;
                            }
                        }
                    }
                }
            }
        }
        return ret;
    }

    private static int isEnabled(Context context, PackageInfo info) {
        int ret = -1;
        if (context != null) {
            try {
                Object /* PackageParser.Package */ pkg = ComponentUtils.parsePackageInfo(info);
                // List<PackageParser.Activity> receivers = pkg.receivers;
                ArrayList<Object> receivers = PackageParserUtils.packageReceivers(pkg);
                for (Object /* PackageParser.Activity */ riobj : receivers) {
                    PackageParserUtils.Activity ri = PackageParserUtils.Activity.fromComponent(riobj);
                    if (ri.intents != null && ri.intents.size() != 0) {
                        for (Object /* PackageParser.ActivityIntentInfo */ aiiobj : ri.intents) {
                            IntentFilter aii = (IntentFilter) aiiobj;
                            if (aii.countActions() > 0) {
                                for (int i = 0; i < aii.countActions(); i++) {
                                    if (aii.getAction(i).equals(Intent.ACTION_BOOT_COMPLETED)) {
                                        ComponentName cn = ri.getComponentName();
                                        ret = (context.getPackageManager().getComponentEnabledSetting(cn) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED) ? 1 : 0;
                                        break;
                                    }
                                }
                            }
                        }

                    }
                }
            } catch (Exception e) {

            }
        }
        return ret;
    }

}
