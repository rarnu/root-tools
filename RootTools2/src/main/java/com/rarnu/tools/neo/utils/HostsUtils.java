package com.rarnu.tools.neo.utils;

import android.content.Context;
import android.os.Environment;
import com.rarnu.tools.neo.root.CommandResult;
import com.rarnu.tools.neo.root.RootUtils;

import java.io.File;
import java.io.IOException;

public class HostsUtils {

    public static final String STANDARD_HOSTS = "127.0.0.1  localhost\n255.255.255.255	broadcasthost\n::1	localhost\nfe80::1%lo0	localhost\n";
    public static final String NOUPDATE_HOSTS = "127.0.0.1 update.miui.com\n";

    public static boolean writeHost(Context ctx, boolean isNoUpdate, boolean isNoAd) {
        boolean ret = false;
        String host = STANDARD_HOSTS;
        if (isNoUpdate) {
            host += NOUPDATE_HOSTS;
        }
        if (isNoAd) {
            String noad = "";
            try {
                noad = FileUtils.readAssetFile(ctx, "hosts_noad");
            } catch (Exception e) {

            }
            host += noad + "\n";
        }
        String tmpDir = Environment.getExternalStorageDirectory().getAbsolutePath();
        File fDir = new File(tmpDir, ".tmp");
        if (!fDir.exists()) {
            fDir.mkdirs();
        }
        String tmpFile = new File(fDir, "hosts").getAbsolutePath();
        try {
            FileUtils.rewriteFile(tmpFile, host);
            RootUtils.mountRW();
            CommandResult result = RootUtils.runCommand(String.format("cp %s %s", tmpFile, "/etc/hosts"), true);
            if (result != null && result.error.equals("")) {
                RootUtils.runCommand(String.format("chmod 755 %s", "/etc/hosts"), true);
            }
            ret = true;
        } catch (Exception e) {

        }
        return ret;
    }

}
