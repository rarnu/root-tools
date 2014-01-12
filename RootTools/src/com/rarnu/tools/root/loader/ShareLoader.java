package com.rarnu.tools.root.loader;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.ShareItem;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class ShareLoader extends BaseLoader<ShareItem> {

    String[] sharePackage = null;
    String[] shareSystem = null;
    String[] shareTitle = null;
    boolean refresh = false;

    public ShareLoader(Context context) {
        super(context);
    }

    public void setData(String[] packages, String[] system, String[] shareTitle) {
        this.sharePackage = packages;
        this.shareSystem = system;
        this.shareTitle = shareTitle;
    }

    public boolean isRefresh() {
        return refresh;
    }

    public void setRefresh(boolean refresh) {
        this.refresh = refresh;
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
            if (sharePackage[i].contains("|")) {
                appinfo = ApkUtils.findApplication(getContext(), sharePackage[i], shareSystem[i].equals("true"));
            } else {
                try {
                    appinfo = getContext().getPackageManager().getApplicationInfo(sharePackage[i], 0);
                } catch (Exception e) {

                }
            }
            ShareItem item = new ShareItem(
                    i,
                    shareTitle[i],
                    appinfo != null ? appinfo.packageName : null
            );
            list.add(item);

        }
        FileUtils.saveListToFile(list, fileName);
        return list;
    }
}
