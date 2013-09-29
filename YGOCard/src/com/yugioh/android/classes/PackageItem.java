package com.yugioh.android.classes;

import java.io.Serializable;

public class PackageItem implements Serializable {

    public boolean isPackageTitle = false;
    public String id = "";
    public String name = "";

    public PackageItem(boolean isPackageTitle, String id, String name) {
        this.isPackageTitle = isPackageTitle;
        this.id = id;
        this.name = name;
    }

}
