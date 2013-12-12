package com.rarnu.tools.root.common;

import java.io.Serializable;

public class ShareItem implements Serializable {

    public String title;
    public String packageName;
    public String className;

    public ShareItem(String title, String packageName, String className) {
        this.title = title;
        this.packageName = packageName;
        this.className = className;
    }

}
