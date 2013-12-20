package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.GoogleInfo;
import com.rarnu.tools.root.common.GooglePackageInfo;
import com.rarnu.tools.root.utils.GoogleUtils;

import java.util.ArrayList;
import java.util.List;

public class GoogleLoader extends BaseLoader<GoogleInfo> {

    private GooglePackageInfo packageItem;

    public GoogleLoader(Context context) {
        super(context);
    }

    public void setPackageItem(GooglePackageInfo packageItem) {
        this.packageItem = packageItem;
    }

    @Override
    public List<GoogleInfo> loadInBackground() {
        return GoogleUtils.getGoogleApps(getContext(), packageItem);
    }
}
