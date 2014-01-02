package com.rarnu.tools.root.common;

import java.io.Serializable;

public class ShareItem implements Serializable {

    public String title;
    public String packageName;
    public boolean installed;
    public int id;

    public ShareItem(int id, String title, String packageName) {
        this.id = id;
        this.title = title;
        this.packageName = packageName;
        installed = packageName != null;
    }

}
