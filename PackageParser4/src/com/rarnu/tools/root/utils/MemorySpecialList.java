package com.rarnu.tools.root.utils;

import com.rarnu.tools.root.common.MemIgnoreInfo;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class MemorySpecialList {

    private static final String PATH_LIST = DirHelper.MEM_DIR + "ignore";
    private static List<MemIgnoreInfo> lstExclude = new ArrayList<MemIgnoreInfo>();

    public static List<MemIgnoreInfo> getExcludeList() {
        return lstExclude;
    }

    public static int inExcludeList(String ns) {
        int ret = -1;

        for (int i = 0; i < lstExclude.size(); i++) {

            if (lstExclude.get(i).namespace.equals(ns)) {
                ret = i;
                break;
            }
        }
        return ret;
    }

    public static boolean isExcludeLocked(String ns) {
        boolean ret = false;
        for (int i = 0; i < lstExclude.size(); i++) {
            if (lstExclude.get(i).namespace.equals(ns)) {
                ret = lstExclude.get(i).locked;
                break;
            }
        }
        return ret;
    }

    public static void loadExcludeList() {
        List<String> list = null;
        try {
            list = FileUtils.readFile(PATH_LIST);
        } catch (IOException e) {

        }

        lstExclude.clear();
        // self
        lstExclude.add(newIgnoreInfo("com.rarnu.tools.root", true));

        // system
        lstExclude.add(newIgnoreInfo("android.process.acore", true));
        lstExclude.add(newIgnoreInfo("system_server", true));
        lstExclude.add(newIgnoreInfo("com.android.systemui", true));
        lstExclude.add(newIgnoreInfo("com.android.phone", true));

        // htc
        lstExclude.add(newIgnoreInfo("com.htc.launcher", true));

        if (list != null && list.size() != 0) {
            for (String s : list) {
                if (inExcludeList(s) == -1) {
                    lstExclude.add(newIgnoreInfo(s, false));
                }
            }
        }

    }

    private static MemIgnoreInfo newIgnoreInfo(String ns, boolean locked) {
        MemIgnoreInfo info = new MemIgnoreInfo();
        info.namespace = ns;
        info.checked = false;
        info.locked = locked;
        return info;
    }

    public static void addExclude(String ns) {
        if (inExcludeList(ns) == -1) {
            MemIgnoreInfo info = new MemIgnoreInfo();
            info.namespace = ns;
            info.checked = false;
            info.locked = false;
            lstExclude.add(info);
        }
    }

    public static void removeExclude(String ns) {
        int idx = inExcludeList(ns);
        if (idx != -1) {
            lstExclude.remove(idx);
        }
    }

    public static void removeExclude(int index) {
        lstExclude.remove(index);
    }

    public static boolean saveExclude() {
        String ignore = "";
        for (MemIgnoreInfo info : lstExclude) {
            ignore += info.namespace + "\n";
        }
        try {
            FileUtils.rewriteFile(PATH_LIST, ignore);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

}
