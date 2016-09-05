package com.rarnu.tools.neo.data;

import android.content.IntentFilter;
import com.rarnu.tools.neo.utils.PackageParserUtils;

import java.util.ArrayList;
import java.util.List;

public class CompInfo {

    /* PackageParser.Component<?> */
    public PackageParserUtils.Component component = null;
    public boolean enabled = false;
    public String fullPackageName = null;

    public String getCompName() {
        return component.className.substring(component.className.lastIndexOf(".") + 1);
    }

    public List<String> getIntents() {
        List<String> result = new ArrayList<>();
        if (component != null && component.intents != null) {
            for (IntentFilter a : component.intents) {
                if (a.countActions() > 0) {
                    for (int i = 0 ; i < a.countActions(); i ++) {
                        result.add(a.getAction(i));
                    }
                }
            }
        }
        return result;
    }

}
