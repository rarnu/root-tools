package com.rarnu.tools.neo.data;

import android.graphics.drawable.Drawable;

/**
 * Created by rarnu on 9/5/16.
 */
public class BanStartInfo extends AppInfo {

    public boolean isBanned = false;
    public boolean isSystem = false;

    public BanStartInfo(String name, Drawable imageId, String packageName, String version, boolean isSystem, boolean isBanned) {
        super(name, imageId, packageName, false, version);
        this.isBanned = isBanned;
        this.isSystem = isSystem;
    }
}
