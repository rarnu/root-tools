package com.rarnu.tools.neo.loader;

import android.content.Context;
import com.rarnu.tools.neo.base.BaseLoader;
import com.rarnu.tools.neo.data.AppInfo;
import com.rarnu.tools.neo.utils.AppUtils;

import java.util.List;

public class AllAppLoader extends BaseLoader<AppInfo> {

    public AllAppLoader(Context context) {
        super(context);
    }

    @Override
    public List<AppInfo> loadInBackground() {
        return AppUtils.getInstalledApps(getContext());
    }
}
