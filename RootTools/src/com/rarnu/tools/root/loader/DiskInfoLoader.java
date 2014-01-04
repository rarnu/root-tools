package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.DiskInfo;
import com.rarnu.tools.root.utils.DiskUtils;

import java.util.List;

public class DiskInfoLoader extends BaseLoader<DiskInfo> {

    public DiskInfoLoader(Context context) {
        super(context);
    }

    @Override
    public List<DiskInfo> loadInBackground() {
        List<DiskInfo> list = null;
        if (getContext() != null) {
            list = DiskUtils.getDiskInfoList();
        }
        return list;
    }

}
