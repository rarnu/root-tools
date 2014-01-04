package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.IptablePackageInfo;
import com.rarnu.tools.root.utils.IptablesUtils;

import java.util.List;

public class FirewallLoader extends BaseLoader<IptablePackageInfo> {

    public FirewallLoader(Context context) {
        super(context);
    }

    @Override
    public List<IptablePackageInfo> loadInBackground() {
        List<IptablePackageInfo> list = null;
        if (getContext() != null) {
            list = IptablesUtils.getApps(getContext(), true);
        }
        return list;
    }
}
