package com.rarnu.tools.neo.data;

import android.graphics.drawable.Drawable;

public class AppInfo {

    public String name = null;
    public Drawable imageId = null;
    public String packageName = null;
    public boolean isDisable = false;
    public String version = null;
    public int versionCode = 0;
    public boolean isSystem = false;
    public boolean isForFreeze = false;

    public AppInfo(String name, Drawable imageId, String packageName, boolean isDisable, String version, int versionCode, boolean isSystem, boolean isForFreeze) {
        this.name = name;
        this.imageId = imageId;
        this.packageName = packageName;
        this.isDisable = isDisable;
        this.version = version;
        this.versionCode = versionCode;
        this.isSystem = isSystem;
        this.isForFreeze = isForFreeze;
    }

}
