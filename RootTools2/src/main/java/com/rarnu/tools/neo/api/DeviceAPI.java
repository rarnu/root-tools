package com.rarnu.tools.neo.api;

import android.content.Context;
import android.content.Intent;
import com.rarnu.tools.neo.fragment.CleanFragment;
import com.rarnu.tools.neo.xposed.XpStatus;

/**
 * Created by rarnu on 11/24/16.
 */
public class DeviceAPI {

    public static final int STATUS_PROGRESS = 0;
    public static final int STATUS_COMPLETE = 1;
    public static final int STATUS_ERROR = 2;

    public static boolean isRejected() {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.isRejected;
        } else {
            return RootAPI.isRejected;
        }
    }

    public static void setRejected(boolean b) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            NativeAPI.isRejected = b;
        } else {
            RootAPI.isRejected = b;
        }
    }

    public static boolean isSystemRW() {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.isSystemRW();
        } else {
            return RootAPI.isSystemRW();
        }
    }

    public static void setSystemRW(boolean b) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            NativeAPI.isSystemRW = b;
        } else {
            RootAPI.isSystemRW = b;
        }
    }


    public static boolean mount() {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.mount();
        } else {
            return RootAPI.mount();
        }
    }

    public static boolean isAppRequiredBySystem(String pkgName) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.isAppRequiredBySystem(pkgName);
        } else {
            return RootAPI.isAppRequiredBySystem(pkgName);
        }
    }

    public static boolean writeFile(Context ctx, String filePath, String text, int perm) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.writeFile(ctx, filePath, text, perm);
        } else {
            return RootAPI.writeFile(ctx, filePath, text, perm);
        }
    }

    public static boolean catFile(String src, String dest, int perm) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.catFile(src, dest, perm);
        } else {
            return RootAPI.catFile(src, dest, perm);
        }
    }

    public static void systemClean(Context ctx) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            NativeAPI.systemClean(ctx);
        } else {
            RootAPI.systemClean(ctx);
        }
    }

    public static boolean freezeComponent(String packageName, String componentName, boolean isFreezed) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.freezeComponent(packageName, componentName, isFreezed);
        } else {
            return RootAPI.freezeComponent(packageName, componentName, isFreezed);
        }
    }

    public static boolean freezeComponents(String packageName, String[] componentNames, boolean isFreezed) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.freezeComponents(packageName, componentNames, isFreezed);
        } else {
            return RootAPI.freezeComponents(packageName, componentNames, isFreezed);
        }
    }

    public static boolean freezeApplication(String packageName, boolean isFreezed) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.freezeApplication(packageName, isFreezed);
        } else {
            return RootAPI.freezeApplication(packageName, isFreezed);
        }
    }

    public static boolean deleteSystemApp(String pkgName) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            return NativeAPI.deleteSystemApp(pkgName);
        } else {
            return RootAPI.deleteSystemApp(pkgName);
        }
    }

    public static void makePreferenceReadable(int sdk, String packageName) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            NativeAPI.makePreferenceReadable(sdk, packageName);
        } else {
            RootAPI.makePreferenceReadable(sdk, packageName);
        }
    }

    public static void killProcess() {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            NativeAPI.killProcess();
        } else {
            RootAPI.killProcess();
        }
    }

    public static void forceDropCache() {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            NativeAPI.forceDropCache();
        } else {
            RootAPI.forceDropCache();
        }
    }

    public static void forceDeleteFile(String path) {
        if (XpStatus.mode == XpStatus.Mode.NDK) {
            NativeAPI.forceDeleteFile(path);
        } else {
            RootAPI.forceDeleteFile(path);
        }
    }

    public static void cleanCallback(Context ctx, int status, String data) {
        Intent inCallback = new Intent(CleanFragment.ACTION_CLEAN_CALLBACK);
        inCallback.putExtra(CleanFragment.KEY_STATUS, status);
        inCallback.putExtra(CleanFragment.KEY_DATA, data);
        ctx.sendBroadcast(inCallback);
    }
}
