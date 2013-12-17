package com.rarnu.tools.root.common;

import android.content.pm.ApplicationInfo;
import android.graphics.drawable.Drawable;


public class IptablePackageInfo {
    public int uid;
    public String names[];
    public boolean selected_wifi;
    public boolean selected_3g;
    public String tostr;
    public ApplicationInfo appinfo;
    public boolean firstseem;

    public IptablePackageInfo() {
    }

    public IptablePackageInfo(int uid, String name, boolean selected_wifi, boolean selected_3g) {
        this.uid = uid;
        this.names = new String[]{name};
        this.selected_wifi = selected_wifi;
        this.selected_3g = selected_3g;
    }

    @Override
    public String toString() {
        if (tostr == null) {
            final StringBuilder s = new StringBuilder();
            for (int i = 0; i < names.length; i++) {
                if (i != 0) s.append(", ");
                s.append(names[i]);
            }
            s.append("\n");
            tostr = s.toString();
        }
        return tostr;
    }

}
