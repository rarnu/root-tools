package com.rarnu.tools.root.common;

import android.content.Context;
import android.content.IntentFilter;
import com.rarnu.tools.root.utils.ComponentUtils;
import com.rarnu.tools.root.utils.PackageParserUtils;

public class CompInfo {

    public PackageParserUtils.Component /* PackageParser.Component<?> */ component;
    public boolean enabled;
    public int position;
    public String fullPackageName;

    public String getCompName() {
        return component.className.substring(component.className.lastIndexOf(".") + 1);
    }

    public boolean isActivity() {
        return (component instanceof PackageParserUtils.Activity);
    }

    public String appendIntents(String str) {
        PackageParserUtils.Activity pa = (PackageParserUtils.Activity) component;
        if (pa.intents != null) {
            if (pa.intents.size() > 0) {
                for (Object aobj : pa.intents) {
                    IntentFilter aii = (IntentFilter) aobj;
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
            ret = ComponentUtils.isServiceRunning(context, component.className);
        }
        return ret;
    }
}
