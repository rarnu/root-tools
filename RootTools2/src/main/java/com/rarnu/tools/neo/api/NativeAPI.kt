package com.rarnu.tools.neo.api

import android.content.Context
import android.content.Intent
import com.rarnu.tools.neo.fragment.CleanFragment

object NativeAPI {

    init {
        System.loadLibrary("cmd")
    }

    var rejected = false
    var systemRW = false

    external fun mount(): Boolean
    external fun isSystemRW(): Boolean
    external fun makePreferenceReadable(sdk: Int, packageName: String?)
    external fun freezeApplication(packageName: String?, isFreezed: Boolean): Boolean
    external fun freezeComponent(packageName: String?, componentName: String?, isFreezed: Boolean): Boolean
    external fun freezeComponents(packageName: String?, componentNames: Array<String>?, isFreezed: Boolean): Boolean
    external fun systemClean(ctx: Context)
    external fun writeFile(ctx: Context, filePath: String?, text: String?, perm: Int): Boolean
    external fun catFile(src: String?, dest: String?, perm: Int): Boolean
    external fun forceDeleteFile(path: String?)
    external fun forceDropCache()
    external fun killProcess()
    external fun deleteSystemApp(pkgName: String?): Boolean
    external fun isAppRequiredBySystem(pkgName: String?): Boolean

    fun cleanCallback(ctx: Context, status: Int, data: String?) {
        DeviceAPI.cleanCallback(ctx, status, data)
    }
}

