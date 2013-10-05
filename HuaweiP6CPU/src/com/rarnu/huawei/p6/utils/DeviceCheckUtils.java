package com.rarnu.huawei.p6.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;

public class DeviceCheckUtils {

    public static boolean isHuaweiP6() {

        String productBoard = getProp("ro.product.board").toLowerCase();
        String productModel = getProp("ro.product.model").toLowerCase();
        String productName = getProp("ro.product.name").toLowerCase();
        String productBrand = getProp("ro.product.brand").toLowerCase();
        boolean ret = false;
        if (productBrand.contains("huawei")) {
            if (productBoard.contains("p6") || productModel.contains("p6") || productName.contains("p6")) {
                ret = true;
            }
        }

        return ret;
    }

    private static String getProp(String name) {
        CommandResult result = RootUtils.runCommand(String.format("getprop %s", name), false);
        String ret = "";
        if (result.error.equals("")) {
            ret = result.result;
        }
        return ret;
    }
}
