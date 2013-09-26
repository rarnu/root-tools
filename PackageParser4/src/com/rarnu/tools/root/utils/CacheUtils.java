package com.rarnu.tools.root.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.CacheInfo;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

public class CacheUtils {

    public static List<CacheInfo> getCacheList() {

        List<CacheInfo> result = null;

        CommandResult cmdResult = RootUtils.runCommand("busybox find /data/data/ -name \"cache\"", true, null);
        if (cmdResult.error.equals("") && cmdResult.result.equals("")) {
            return result;
        }

        cmdResult = RootUtils.runCommand("busybox find /data/data/ -name \"cache\" | busybox xargs du -s", true, null);
        if (cmdResult.error.equals("")) {
            String cacheString = cmdResult.result;
            cacheString = cacheString.replace("\t", " ");
            cacheString = cacheString.replace("/data/data/", "").replace("/cache", "").replace("\n", "|");
            cacheString = cacheString.replaceAll("\\s+", " ");
            String[] lines = cacheString.split("\\|");

            if (lines != null && lines.length != 0) {
                result = new ArrayList<CacheInfo>();
                for (String s : lines) {
                    if (s.trim().equals("")) {
                        continue;
                    }
                    CacheInfo info = CacheInfo.parseString(s);
                    if (info != null) {
                        result.add(info);
                    }
                }
            }
        }
        return result;
    }

    public static boolean cleanCache(CacheInfo info) {
        String path = "rm -r /data/data/%s/cache";
        CommandResult result = RootUtils.runCommand(String.format(path, info.namespace), true, null);
        return result.error.equals("");
    }

    public static boolean cleanAllCache() {
        String cmd = "busybox find /data/data/ -name \"cache\" | busybox xargs rm -r";
        CommandResult result = RootUtils.runCommand(cmd, true, null);
        return result.error.equals("");
    }

    public static String countCache(List<CacheInfo> list) {
        if (list == null || list.size() == 0) {
            return "0.0M";
        }
        double count = 0D;
        String size = "";
        double sizeD = 0D;
        for (CacheInfo info : list) {
            size = info.cacheSize;
            try {
                sizeD = Double.parseDouble(size.substring(0, size.length() - 1));
            } catch (Exception e) {
                sizeD = 0D;
            }

            count += sizeD;
        }

        count /= 1024;
        String ret = new DecimalFormat("#.##").format(count) + "M";
        return ret;
    }
}
