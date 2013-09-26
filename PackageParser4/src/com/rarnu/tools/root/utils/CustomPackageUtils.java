package com.rarnu.tools.root.utils;

import com.rarnu.tools.root.common.CustomPackageInfo;

import java.util.ArrayList;
import java.util.List;

public class CustomPackageUtils {

    private static final String PATH_LIST = DirHelper.ROOT_DIR + "packages";
    private static List<CustomPackageInfo> lstCustomPackage = new ArrayList<CustomPackageInfo>();

    public static List<CustomPackageInfo> getCustomPackageList() {
        return lstCustomPackage;
    }

    public static void addCustomPackage(String title, String namespace) {
        if (customPackageIndex(namespace) == -1) {
            CustomPackageInfo info = new CustomPackageInfo();
            info.title = title;
            info.namespace = namespace;
            lstCustomPackage.add(info);
        }
    }

    public static void removeCustomPackage(String namespace) {
        int idx = customPackageIndex(namespace);
        if (idx != -1) {
            lstCustomPackage.remove(idx);
        }
    }

    public static void removeCustomPackage(int idx) {
        lstCustomPackage.remove(idx);
    }

    public static int customPackageIndex(String namespace) {
        int ret = -1;
        for (int i = 0; i < lstCustomPackage.size(); i++) {
            if (lstCustomPackage.get(i).namespace.equals(namespace)) {
                ret = i;
                break;
            }
        }
        return ret;
    }

    // title||namespace
    public static void loadCustomPackages() {
        List<String> lst = null;
        try {
            lst = FileUtils.readFile(PATH_LIST);
        } catch (Exception e) {

        }
        lstCustomPackage.clear();
        if (lst != null && lst.size() != 0) {
            for (String s : lst) {
                lstCustomPackage.add(buildCustomPackage(s));
            }
        }
    }

    private static CustomPackageInfo buildCustomPackage(String s) {
        CustomPackageInfo info = new CustomPackageInfo();
        String[] ss = s.split("\\|\\|");
        info.title = ss[0];
        info.namespace = ss[1];
        return info;
    }

    public static boolean saveCustomPackages() {
        String pkg = "";
        for (CustomPackageInfo info : lstCustomPackage) {
            pkg += info.title + "||" + info.namespace + "\n";
        }
        try {
            FileUtils.rewriteFile(PATH_LIST, pkg);
            return true;
        } catch (Exception e) {
            return false;
        }
    }
}
