package com.rarnu.tools.root.common;

import android.content.Context;
import android.content.pm.PackageParser;
import com.rarnu.tools.root.utils.ComponentUtils;

public class CompInfo {

    public PackageParser.Component<?> component;
    public boolean enabled;
    public int position;
    public String fullPackageName;

    public String getCompName() {
        return ((PackageParser.Component<?>) component).className.substring(((PackageParser.Component<?>) component).className.lastIndexOf(".") + 1);
    }

    public boolean isActivity() {
        return (component instanceof PackageParser.Activity);
    }

    public String appendIntents(String str) {
        PackageParser.Activity pa = (PackageParser.Activity) component;
        if (pa.intents != null) {
            if (pa.intents.size() > 0) {
                for (PackageParser.ActivityIntentInfo aii : pa.intents) {
                    if (aii.countActions() > 0) {
                        for (int i = 0; i < aii.countActions(); i++) {
                            str += aii.getAction(i).substring(aii.getAction(i).lastIndexOf(".") + 1).replace("_", " ").toLowerCase() + "<br />";
                        }
                    }
                }
            }
        }
        return str;
    }

    public boolean isServiceRunning(Context context) {
        boolean ret = false;
        if (!isActivity()) {
            ret = ComponentUtils.isServiceRunning(context, ((PackageParser.Component<?>) component).className);
        }
        return ret;
    }
}
