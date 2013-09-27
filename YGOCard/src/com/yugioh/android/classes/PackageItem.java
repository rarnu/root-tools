package com.yugioh.android.classes;

public class PackageItem {

    public boolean isPackageTitle = false;
    public String id = "";
    public String name = "";

    public PackageItem(boolean isPackageTitle, String id, String name) {
        this.isPackageTitle = isPackageTitle;
        this.id = id;
        this.name = name;
    }

}
