package com.rarnu.tools.root.utils;

import android.Manifest;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageParser;
import android.util.DisplayMetrics;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.AutobootInfo;

import java.util.ArrayList;
import java.util.List;

public class AutobootUtils {

    public static List<AutobootInfo> getAuobootApps(Context context, DisplayMetrics dm) {
        List<PackageInfo> apps = GlobalInstance.pm.getInstalledPackages(0);
        List<AutobootInfo> list = new ArrayList<AutobootInfo>();
        int status = -1;
        for (PackageInfo pi : apps) {
            if (GlobalInstance.pm.checkPermission(Manifest.permission.RECEIVE_BOOT_COMPLETED, pi.packageName) == PackageManager.PERMISSION_GRANTED) {
                status = isEnabled(pi, dm);
                if (status != -1) {
                    AutobootInfo info = new AutobootInfo();
                    info.info = pi;
                    info.enabled = (status != 0);
                    list.add(info);
                }
            }
        }
        return list;
    }

    public static boolean switchAutoboot(Context context, AutobootInfo info, DisplayMetrics dm, boolean enabled) {
        boolean ret = true;
        boolean operatingResult = true;
        PackageParser.Package pkg = ComponentUtils.parsePackageInfo(info.info, dm);
        List<PackageParser.Activity> receivers = pkg.receivers;
        for (PackageParser.Activity ri : receivers) {
            if (ri.intents != null && ri.intents.size() != 0) {
                for (PackageParser.ActivityIntentInfo aii : ri.intents) {
                    if (aii.countActions() > 0) {
                        for (int i = 0; i < aii.countActions(); i++) {
                            aii.getAction(i).equals(Intent.ACTION_BOOT_COMPLETED);
                            if (enabled) {
                                operatingResult = ComponentUtils.doEnabledComponent(ri.getComponentName());
                            } else {
                                operatingResult = ComponentUtils.doDisableComponent(ri.getComponentName());
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
        return ret;
    }

    private static int isEnabled(PackageInfo info, DisplayMetrics dm) {
        int ret = -1;
        PackageParser.Package pkg = ComponentUtils.parsePackageInfo(info, dm);
        List<PackageParser.Activity> receivers = pkg.receivers;
        for (PackageParser.Activity ri : receivers) {
            if (ri.intents != null && ri.intents.size() != 0) {
                for (PackageParser.ActivityIntentInfo aii : ri.intents) {
                    if (aii.countActions() > 0) {
                        for (int i = 0; i < aii.countActions(); i++) {
                            if (aii.getAction(i).equals(Intent.ACTION_BOOT_COMPLETED)) {
                                ret = (GlobalInstance.pm.getComponentEnabledSetting(ri.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED) ? 1 : 0;
                                break;
                            }
                        }
                    }
                }

            }
        }
        return ret;
    }

}
