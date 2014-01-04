package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

import java.util.List;

public class BackupLoader extends BaseLoader<DataappInfo> {

    public BackupLoader(Context context) {
        super(context);
    }

    @Override
    public List<DataappInfo> loadInBackground() {
        List<DataappInfo> list = null;
        if (getContext() != null) {
            list = ApkUtils.getInstalledApps(getContext(), false);
        }
        return list;
    }
}
