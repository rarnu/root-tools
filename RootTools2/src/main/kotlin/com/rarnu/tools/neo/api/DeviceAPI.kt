package com.rarnu.tools.neo.api

import android.content.Context
import android.content.Intent
import com.rarnu.tools.neo.activity.CleanActivity

/**
 * Created by rarnu on 11/24/16.
 */
object DeviceAPI {

    const val STATUS_PROGRESS = 0
    const val STATUS_COMPLETE = 1
    const val STATUS_ERROR = 2

    var isRejected: Boolean
        get() = NativeAPI.rejected
        set(b) {
            NativeAPI.rejected = b
        }

    var isSystemRW: Boolean
        get() = NativeAPI.isSystemRW()
        set(b) {
            NativeAPI.systemRW = b
        }

    fun mount() = NativeAPI.mount()
    fun isAppRequiredBySystem(pkgName: String?) = NativeAPI.isAppRequiredBySystem(pkgName)
    fun writeFile(ctx: Context, filePath: String, text: String, perm: Int) = NativeAPI.writeFile(ctx, filePath, text, perm)
    fun catFile(src: String?, dest: String?, perm: Int) = NativeAPI.catFile(src, dest, perm)
    fun systemClean(ctx: Context?) = NativeAPI.systemClean(ctx)
    fun freezeComponent(packageName: String?, componentName: String?, isFreezed: Boolean) = NativeAPI.freezeComponent(packageName, componentName, isFreezed)
    fun freezeComponents(packageName: String?, componentNames: Array<String>?, isFreezed: Boolean) = NativeAPI.freezeComponents(packageName, componentNames, isFreezed)
    fun freezeApplication(packageName: String?, isFreezed: Boolean) = NativeAPI.freezeApplication(packageName, isFreezed)
    fun deleteSystemApp(pkgName: String?) = NativeAPI.deleteSystemApp(pkgName)
    fun makePreferenceReadable(sdk: Int, packageName: String?) = NativeAPI.makePreferenceReadable(sdk, packageName)
    fun killProcess() = NativeAPI.killProcess()
    fun forceDropCache() = NativeAPI.forceDropCache()
    fun forceDeleteFile(path: String) = NativeAPI.forceDeleteFile(path)
    fun cleanCallback(ctx: Context?, status: Int, data: String?) {
        val inCallback = Intent(CleanActivity.ACTION_CLEAN_CALLBACK)
        inCallback.putExtra(CleanActivity.KEY_STATUS, status)
        inCallback.putExtra(CleanActivity.KEY_DATA, data)
        ctx?.sendBroadcast(inCallback)
    }
}
