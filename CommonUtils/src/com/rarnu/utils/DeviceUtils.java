package com.rarnu.utils;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.telephony.TelephonyManager;
import android.util.DisplayMetrics;
import com.rarnu.command.RootUtils;
import com.rarnu.utils.common.DeviceInfo;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.List;

public class DeviceUtils {

    public static final String RO_BUILD_ID = "ro.build.id";
    public static final String RO_BUILD_VERSION_SDK = "ro.build.version.sdk";
    public static final String RO_BUILD_VERSION_RELEASE = "ro.build.version.release";
    public static final String RO_PRODUCT_MODEL = "ro.product.model";
    public static final String RO_PRODUCT_BRAND = "ro.product.brand";
    public static final String RO_PRODUCT_NAME = "ro.product.name";
    public static final String RO_PRODUCT_DEVICE = "ro.product.device";
    public static final String RO_PRODUCT_BOARD = "ro.product.board";
    public static final String RO_PRODUCT_CPU_ABI = "ro.product.cpu.abi";
    public static final String RO_PRODUCT_CPU_ABI2 = "ro.product.cpu.abi2";
    public static final String RO_PRODUCT_MANUFACTURER = "ro.product.manufacturer";
    public static final String RO_BOARD_PLATFORM = "ro.board.platform";
    public static final String RO_BUILD_DESCRIPTION = "ro.build.description";
    public static final String RO_PRODUCT_VERSION = "ro.product.version";
    public static final String RO_MIUI_UI_VERSION_CODE = "ro.miui.ui.version.code";
    public static final String RO_MIUI_UI_VERSION_NAME = "ro.miui.ui.version.name";
    private static final String BUILD_PROP = "/system/build.prop";
    private static List<String> buildProp = null;

    public static DeviceInfo getDeviceInfo() {
        DeviceInfo info = null;
        if (buildProp == null) {
            try {
                buildProp = FileUtils.readFile(BUILD_PROP);
            } catch (IOException ioe) {
                return null;
            }
        }

        info = new DeviceInfo();
        info.roBuildId = findPropValue(RO_BUILD_ID);
        info.roBuildVersionSdk = findPropValue(RO_BUILD_VERSION_SDK);
        info.roBuildVersionRelease = findPropValue(RO_BUILD_VERSION_RELEASE);
        info.roProductModel = findPropValue(RO_PRODUCT_MODEL);
        info.roProductBrand = findPropValue(RO_PRODUCT_BRAND);
        info.roProductName = findPropValue(RO_PRODUCT_NAME);
        info.roProductDevice = findPropValue(RO_PRODUCT_DEVICE);
        info.roProductBoard = findPropValue(RO_PRODUCT_BOARD);
        info.roProductCpuAbi = findPropValue(RO_PRODUCT_CPU_ABI);
        info.roProductCpuAbi2 = findPropValue(RO_PRODUCT_CPU_ABI2);
        info.roProductManufacturer = findPropValue(RO_PRODUCT_MANUFACTURER);
        info.roBoardPlatform = findPropValue(RO_BOARD_PLATFORM);
        info.roBuildDescription = findPropValue(RO_BUILD_DESCRIPTION);
        info.roProductVersion = findPropValue(RO_PRODUCT_VERSION);

        return info;
    }

    public static String getBuildProp(String key) {

        if (buildProp == null) {
            try {
                buildProp = FileUtils.readFile(BUILD_PROP);
            } catch (IOException ioe) {
                return null;
            }
        }
        return findPropValue(key);
    }

    public static String getDeviceUniqueId(Context context) {
        TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
        return tm.getDeviceId() + "_" + tm.getSubscriberId();
    }

    private static String findPropValue(String key) {
        String tmp;
        String val = null;
        int idx = -1;
        for (String s : buildProp) {
            idx = s.indexOf("=");
            if (idx < 0) {
                continue;
            }
            tmp = s.substring(0, idx);
            if (tmp.equals(key)) {
                val = s.substring(idx + 1);
                break;
            }
        }
        return val;
    }

    public static int getAppVersionCode(Context context, String filePath) {
        int versionCode = 0;
        try {
            PackageInfo pi = context.getPackageManager().getPackageArchiveInfo(filePath, 0);
            versionCode = pi.versionCode;
        } catch (Exception e) {

        }
        return versionCode;
    }

    public static int getAppVersionCode(Context context) {
        return getAppVersionCode(context, (ApplicationInfo) null);
    }

    public static int getAppVersionCode(Context context, ApplicationInfo info) {
        int versionCode = 0;
        String packageName = "";
        if (info != null) {
            packageName = info.packageName;
        } else {
            packageName = context.getPackageName();
        }
        try {
            PackageInfo pi = context.getPackageManager().getPackageInfo(packageName, 0);
            versionCode = pi.versionCode;
        } catch (Exception e) {

        }
        return versionCode;
    }

    public static String getAppVersionName(Context context) {
        return getAppVersionName(context, null);
    }

    public static String getAppVersionName(Context context, ApplicationInfo info) {
        String versionName = "";
        String packageName = "";
        if (info != null) {
            packageName = info.packageName;
        } else {
            packageName = context.getPackageName();
        }
        try {
            PackageInfo pi = context.getPackageManager().getPackageInfo(packageName, 0);
            versionName = pi.versionName;

        } catch (Exception e) {

        }
        return versionName;
    }

    public static int getFitable(DisplayMetrics metric) {
        getDeviceInfo();

        int result = 5;
        String sdkVer = getBuildProp(RO_BUILD_VERSION_SDK);
        int sdk = Integer.parseInt(sdkVer);
        if (sdk >= 8 && sdk != 11 && sdk != 12 && sdk != 13) {
            result++;
        } else {
            result--;
        }

        String cpu = getBuildProp(RO_PRODUCT_CPU_ABI);
        if (cpu.toLowerCase().contains("armeabi")) {
            result++;
        } else {
            result--;
        }

        String miuiCode = getBuildProp(RO_MIUI_UI_VERSION_CODE);
        String miuiName = getBuildProp(RO_MIUI_UI_VERSION_NAME);
        if (miuiCode != null && miuiName != null && !miuiCode.equals("") && !miuiName.equals("")) {
            result++;
        }

        if (metric.widthPixels >= 480 && metric.heightPixels >= 800) {
            result++;
        } else {
            result--;
        }

        if (metric.widthPixels >= 720 && metric.heightPixels >= 1280) {
            result++;
        }

        if (result < 1) {
            result = 1;
        }
        if (result > 9) {
            result = 9;
        }
        return result;
    }

    public static void reboot() {
        RootUtils.runCommand("busybox reboot -f", true, null);
    }

    public static boolean isMIUI() {
        boolean ret = false;
        String miuiCode = getBuildProp(RO_MIUI_UI_VERSION_CODE);
        String miuiName = getBuildProp(RO_MIUI_UI_VERSION_NAME);
        if (miuiCode != null && miuiName != null && !miuiCode.equals("") && !miuiName.equals("")) {
            ret = true;
        }
        return ret;
    }

    public static boolean isMiuiV6() {
        int ver = 0;
        try {
            Class<?> sys = Class.forName("android.os.SystemProperties");
            Method m = sys.getDeclaredMethod("get", String.class);
            String v = (String) m.invoke(sys, "ro.miui.ui.version.code");
            ver = Integer.parseInt(v);
        } catch (Exception e) {

        }
        return ver >= 4;
    }

    public static boolean isBaiduRom(Context context) {
        boolean ret = false;
        try {
            PackageManager pm = context.getPackageManager();
            ApplicationInfo infoService = pm.getApplicationInfo("com.baidu.bsf.service", 0);
            ApplicationInfo infoSystem = pm.getApplicationInfo("com.baidu.bsf.system", 0);
            ret = (infoService != null && infoSystem != null);
        } catch (Exception e) {

        }

        return ret;
    }
}
