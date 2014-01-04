package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.utils.DIPairUtils;

import java.util.List;

public class HostsLoader extends BaseLoader<HostRecordInfo> {

    public HostsLoader(Context context) {
        super(context);
    }

    @Override
    public List<HostRecordInfo> loadInBackground() {
        List<HostRecordInfo> list = null;
        if (getContext() != null) {
            list = DIPairUtils.getHostList();

        }
        return list;
    }

}
