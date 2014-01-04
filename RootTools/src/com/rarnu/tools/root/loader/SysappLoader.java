package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

import java.util.List;

public class SysappLoader extends BaseLoader<SysappInfo> {

    public SysappLoader(Context context) {
        super(context);
    }

    @Override
    public List<SysappInfo> loadInBackground() {
        List<SysappInfo> list = null;
        if (getContext() != null) {
            list = ApkUtils.getSystemApps(getContext());
        }
        return list;
    }

}
