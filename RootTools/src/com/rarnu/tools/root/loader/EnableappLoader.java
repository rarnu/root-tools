package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

import java.util.List;

public class EnableappLoader extends BaseLoader<EnableappInfo> {

    public EnableappLoader(Context context) {
        super(context);
    }

    @Override
    public List<EnableappInfo> loadInBackground() {
        List<EnableappInfo> list = null;
        if (getContext() != null) {
            list = ApkUtils.getEnabledApplications(getContext());
        }
        return list;
    }

}
