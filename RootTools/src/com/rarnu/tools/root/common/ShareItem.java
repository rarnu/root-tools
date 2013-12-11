package com.rarnu.tools.root.common;

import android.graphics.drawable.Drawable;

public class ShareItem {

    public Drawable icon;
    public String title;

    public String packageName;
    public String className;

    public ShareItem(Drawable icon, String title, String packageName, String className) {
        this.icon = icon;
        this.title = title;
        this.packageName = packageName;
        this.className = className;
    }

}
