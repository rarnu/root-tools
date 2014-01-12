package com.rarnu.tools.root.loader;

import android.content.Context;
import android.content.pm.PackageInfo;
import com.rarnu.devlib.base.BaseLoader;

import java.util.List;

public class CompLoader extends BaseLoader<PackageInfo> {

    public CompLoader(Context context) {
        super(context);
    }

    @Override
    public List<PackageInfo> loadInBackground() {
        List<PackageInfo> list = null;
        if (getContext() != null) {
            try {
                list = getContext().getPackageManager().getInstalledPackages(0);
            } catch (Exception e) {

            }
        }
        return list;
    }

}
