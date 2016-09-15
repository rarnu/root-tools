package com.rarnu.tools.neo.utils;

import android.content.Context;
import com.rarnu.tools.neo.api.NativeAPI;

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

        try {
            NativeAPI.mount();
            ret = NativeAPI.writeFile(ctx, "/etc/hosts", host, 755);
        } catch (Throwable th) {
        }
        return ret;
    }

}
