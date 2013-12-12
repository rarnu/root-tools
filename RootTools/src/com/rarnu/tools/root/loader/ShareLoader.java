package com.rarnu.tools.root.loader;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.ShareItem;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class ShareLoader extends BaseLoader<ShareItem> {

    String[] sharePackage = null;
    String[] shareClass = null;
    String[] shareSystem = null;

    boolean refresh = false;

    public ShareLoader(Context context) {
        super(context);
    }

    public void setData(String[] packages, String[] classes, String[] system) {
        this.sharePackage = packages;
        this.shareClass = classes;
        this.shareSystem = system;
    }

    public void setRefresh(boolean refresh) {
        this.refresh = refresh;
    }

    public boolean isRefresh() {
        return refresh;
    }

    @Override
    @SuppressWarnings("unchecked")
    public List<ShareItem> loadInBackground() {
        List<ShareItem> list = null;
        ApplicationInfo appinfo = null;

        String fileName = DirHelper.TEMP_DIR + "share";
        if (!refresh && new File(fileName).exists()) {
            list = (List<ShareItem>) FileUtils.loadListFromFile(fileName);
            if (list != null) {
                return list;
            }
        }

        refresh = true;
        list = new ArrayList<ShareItem>();

        for (int i = 0; i < sharePackage.length; i++) {
            appinfo = null;
            if (sharePackage[i].contains("*")) {
                appinfo = ApkUtils.findApplication(getContext(), sharePackage[i], shareSystem[i].equals("true"));
            } else {
                try {
                    appinfo = GlobalInstance.pm.getApplicationInfo(sharePackage[i], 0);
                } catch (Exception e) {

                }
            }
            if (appinfo != null) {
                ShareItem item = new ShareItem(
                        GlobalInstance.pm.getApplicationLabel(appinfo).toString(),
                        appinfo.packageName,
                        shareClass[i]
                );
                list.add(item);
            }
        }
        FileUtils.saveListToFile(list, fileName);
        return list;
    }
}
