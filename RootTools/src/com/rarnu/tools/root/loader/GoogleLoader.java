package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.GoogleInfo;
import com.rarnu.tools.root.common.GooglePackageInfo;
import com.rarnu.tools.root.utils.GoogleUtils;

import java.util.List;

public class GoogleLoader extends BaseLoader<GoogleInfo> {

    private GooglePackageInfo packageItem;
    private int sdkVer;

    public GoogleLoader(Context context) {
        super(context);
    }

    public void setData(GooglePackageInfo packageItem, int sdkVer) {
        this.packageItem = packageItem;
        this.sdkVer = sdkVer;
    }

    @Override
    public List<GoogleInfo> loadInBackground() {
        List<GoogleInfo> list = null;
        if (getContext() != null) {
            list = GoogleUtils.getGoogleApps(getContext(), packageItem, sdkVer);
        }
        return list;
    }
}
