package com.yugioh.android.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.utils.FileUtils;
import com.yugioh.android.classes.PackageItem;
import com.yugioh.android.define.PathDefine;
import com.yugioh.android.utils.YGOAPI;

import java.util.List;

public class PackageLoader extends BaseLoader<PackageItem> {

    private boolean refresh = false;

    public PackageLoader(Context context) {
        super(context);
    }

    public void setRefresh(boolean refresh) {
        this.refresh = refresh;
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<PackageItem> loadInBackground() {
        List<PackageItem> list = null;
        if (refresh) {
            refresh = false;
            list = YGOAPI.getPackageList();
            if (list != null && list.size() != 0) {
                FileUtils.saveListToFile(list, PathDefine.PACK_LIST);
            } else {
                list = (List<PackageItem>) FileUtils.loadListFromFile(PathDefine.PACK_LIST);
            }
        } else {
            list = (List<PackageItem>) FileUtils.loadListFromFile(PathDefine.PACK_LIST);
            if (list == null) {
                list = YGOAPI.getPackageList();
                if (list != null) {
                    FileUtils.saveListToFile(list, PathDefine.PACK_LIST);
                }
            }
        }

        return list;
    }
}
