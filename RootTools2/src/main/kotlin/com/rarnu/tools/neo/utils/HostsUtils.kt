package com.rarnu.tools.neo.utils

import android.content.Context
import com.rarnu.base.utils.FileUtils
import com.rarnu.tools.neo.api.DeviceAPI

object HostsUtils {

    private const val STANDARD_HOSTS = "127.0.0.1  localhost\n255.255.255.255	broadcasthost\n::1	localhost\nfe80::1%lo0	localhost\n"
    private const val NOUPDATE_HOSTS = "127.0.0.1 update.miui.com\n"

    fun writeHost(ctx: Context, isNoUpdate: Boolean, isNoAd: Boolean): Boolean {
        var ret = false
        var host = STANDARD_HOSTS
        if (isNoUpdate) {
            host += NOUPDATE_HOSTS
        }
        if (isNoAd) {
            var noad: String? = ""
            try {
                noad = FileUtils.readAssetFile(ctx, "hosts_noad")
            } catch (e: Exception) {

            }
            host += noad + "\n"
        }

        try {
            DeviceAPI.mount()
            ret = DeviceAPI.writeFile(ctx, "/etc/hosts", host, 755)
        } catch (th: Throwable) {
        }

        return ret
    }

}
