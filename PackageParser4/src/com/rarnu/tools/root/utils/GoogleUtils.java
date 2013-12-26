package com.rarnu.tools.root.utils;

import android.content.Context;
import com.rarnu.tools.root.common.GoogleInfo;
import com.rarnu.tools.root.common.GooglePackageInfo;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class GoogleUtils {

    public static List<GoogleInfo> getGoogleApps(Context context, GooglePackageInfo packageInfo, int sdkVer) {
        List<GoogleInfo> list = new ArrayList<GoogleInfo>();
        for (String apk : packageInfo.apks) {
            GoogleInfo item = new GoogleInfo();
            item.fileName = apk;
            item.type = 0;
            item.status = getGoogleInfoStatus(item, sdkVer);
            item.optional = false;
            list.add(item);
        }
        for (String apk : packageInfo.apks_optional) {
            GoogleInfo item = new GoogleInfo();
            item.fileName = apk;
            item.type = 0;
            item.status = getGoogleInfoStatus(item, sdkVer);
            item.optional = true;
            list.add(item);
        }
        for (String jar : packageInfo.jars) {
            GoogleInfo item = new GoogleInfo();
            item.fileName = jar;
            item.type = 1;
            item.status = getGoogleInfoStatus(item, sdkVer);
            item.optional = false;
            list.add(item);
        }
        for (String lib : packageInfo.libs) {
            GoogleInfo item = new GoogleInfo();
            item.fileName = lib;
            item.type = 2;
            item.status = getGoogleInfoStatus(item, sdkVer);
            item.optional = false;
            list.add(item);
        }
        for (String xml : packageInfo.xmls) {
            GoogleInfo item = new GoogleInfo();
            item.fileName = xml.substring(xml.indexOf("/") + 1);
            item.path = xml.substring(0, xml.indexOf("/"));
            item.type = 3;
            item.status = getGoogleInfoStatus(item, sdkVer);
            item.optional = false;
            list.add(item);
        }
        return list;
    }

    private static int getGoogleInfoStatus(GoogleInfo item, int sdkVer) {
        int ret = -1;
        String filePath = "";
        switch (item.type) {
            case 0:
                filePath = "/system/app/" + item.fileName;
                break;
            case 1:
                filePath = "/system/framework/" + item.fileName;
                break;
            case 2:
                filePath = "/system/lib/" + item.fileName;
                break;
            case 3:
                filePath = "/system/etc/" + item.path + "/" + item.fileName;
                break;
        }
        File f = new File(filePath);
        if (!f.exists()) {
            ret = 1;
        } else {

            String newFilePath = "";
            switch (item.type) {
                case 0:
                case 1:
                case 2:
                    newFilePath = DirHelper.GOOGLE_DIR + sdkVer + "/" + item.fileName;
                    break;
                case 3:
                    newFilePath = DirHelper.GOOGLE_DIR + sdkVer + "/" + item.path + "/" + item.fileName;
                    break;
            }
            File newFile = new File(newFilePath);
            if (newFile.exists()) {
                if (f.length() != newFile.length()) {
                    ret = 2;
                } else {
                    ret = 0;
                }
            }
        }
        return ret;
    }

    public static boolean isAllFilesCorrect(List<GoogleInfo> list) {
        boolean ret = true;
        for (GoogleInfo gi : list) {
            if (gi.status == 2) {
                ret = false;
                break;
            }
            if (gi.status != 0 && !gi.optional) {
                ret = false;
                break;
            }
        }
        return ret;
    }

    public static boolean isAllOptionalFilesCorrect(List<GoogleInfo> list) {
        boolean ret = true;
        for (GoogleInfo gi : list) {
            if (gi.optional) {
                if (gi.status != 0) {
                    ret = false;
                    break;
                }
            }
        }
        return ret;
    }

    public static List<GoogleInfo> getInstallFileList(List<GoogleInfo> list, boolean overrideBroken, boolean installOptional, int mode) {
        List<GoogleInfo> result = new ArrayList<GoogleInfo>();
        for (GoogleInfo gi : list) {
            if (mode == 1) {
                if (gi.optional && gi.status != 0 && installOptional) {
                    result.add(gi);
                }
            } else {
                if (gi.status == 2 && overrideBroken) {
                    result.add(gi);
                } else if (gi.status == 1 && gi.optional && installOptional) {
                    result.add(gi);
                } else if (gi.status == 1 && !gi.optional) {
                    result.add(gi);
                }
            }
        }
        return result;
    }
}
