package com.rarnu.tools.neo.loader;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import com.rarnu.tools.neo.base.BaseLoader;
import com.rarnu.tools.neo.data.CompInfo;
import com.rarnu.tools.neo.utils.ComponentUtils;
import com.rarnu.tools.neo.utils.PackageParserUtils;

import java.util.List;

/**
 * Created by rarnu on 9/2/16.
 */
public class ComponentLoader extends BaseLoader<CompInfo> {

    private int type = 0;
    private String pkg = null;

    public ComponentLoader(Context context) {
        super(context);
    }

    @Override
    public List<CompInfo> loadInBackground() {
        List<CompInfo> list = null;
        try {
            ApplicationInfo info = getContext().getPackageManager().getApplicationInfo(pkg, 0);
            PackageParserUtils ppu = new PackageParserUtils();
            Object obj = ppu.parsePackage(info.publicSourceDir, 0);
            switch (type) {
                case 0:
                    list = ComponentUtils.getActivityList(getContext(), obj);
                    break;
                case 1:
                    list = ComponentUtils.getServiceList(getContext(), obj);
                    break;
                case 2:
                    list = ComponentUtils.getReceiverList(getContext(), obj);
                    break;
                case 3:
                    list = ComponentUtils.getProviderList(getContext(), obj);
                    break;
            }
        } catch (Exception e) {
        }
        return list;
    }

    public void startLoading(String pkg, int type) {
        this.type = type;
        this.pkg = pkg;
        startLoading();
    }
}
