package com.rarnu.tools.neo.api;

import android.content.Context;

/**
 * Created by rarnu on 11/21/16.
 */
public class RootAPI {

    public static boolean isRejected = false;
    public static boolean isSystemRW = false;

    public static boolean mount() {
        // TODO: mount
        return false;
    }
    public static boolean isSystemRW() {
        // TODO: isSystemRW
        return false;
    }
    public static void makePreferenceReadable(int sdk, String packageName) {
        // TODO: makePreferenceReadable
    }
    public static boolean freezeApplication(String packageName, boolean isFreezed) {
        // TODO: freezeApplication
        return false;
    }
    public static boolean freezeComponent(String packageName, String componentName, boolean isFreezed) {
        // TODO: freezeComponent
        return false;
    }
    public static boolean freezeComponents(String packageName, String[] componentNames, boolean isFreezed) {
        // TODO: freezeComponents
        return false;
    }
    public static void systemClean(Context ctx) {
        // TODO: systemClean
    }
    public static boolean writeFile(Context ctx, String filePath, String text, int perm) {
        // TODO: writeFile
        return false;
    }
    public static boolean catFile(String src, String dest, int perm) {
        // TODO: catFile
        return false;
    }
    public static void forceDeleteFile(String path) {
        // TODO: forceDeleteFile
    }
    public static void forceDropCache() {
        // TODO: forceDropCache
    }
    public static void killProcess() {
        // TODO: killProcess
    }
    public static boolean deleteSystemApp(String pkgName) {
        // TODO: deleteSystemApp
        return false;
    }
    public static boolean isAppRequiredBySystem(String pkgName) {
        // TODO: isAppRequiredBySystem
        return false;
    }

    public void cleanCallback(Context ctx, int status, String data) {
        DeviceAPI.cleanCallback(ctx, status, data);
    }

}
