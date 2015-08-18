package com.rarnu.utils;

import android.content.Context;
import android.content.pm.PackageManager;

/**
 * Created by rarnu on 3/19/15.
 */
public class PermissionUtils {

    public static boolean hasPermissionDeclare(Context context, String perm) {
        PackageManager pm = context.getPackageManager();
        boolean permission = (PackageManager.PERMISSION_GRANTED == pm.checkPermission(perm, "packageName"));
        return permission;
    }
}
