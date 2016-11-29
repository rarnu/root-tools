package com.rarnu.tools.neo.api

import android.content.Context
import android.content.Intent
import com.rarnu.tools.neo.fragment.CleanFragment
import com.rarnu.tools.neo.xposed.XpStatus

/**
 * Created by rarnu on 11/24/16.
 */
object DeviceAPI {

    val STATUS_PROGRESS = 0
    val STATUS_COMPLETE = 1
    val STATUS_ERROR = 2

    var isRejected: Boolean
        get() {
            if (XpStatus.mode === XpStatus.Mode.NDK) {
                return NativeAPI.rejected
            } else {
                return RootAPI.rejected
            }
        }
        set(b) = if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.rejected = b
        } else {
            RootAPI.rejected = b
        }

    var isSystemRW: Boolean
        get() {
            if (XpStatus.mode === XpStatus.Mode.NDK) {
                return NativeAPI.isSystemRW()
            } else {
                return RootAPI.isSystemRW()
            }
        }
        set(b) = if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.systemRW = b
        } else {
            RootAPI.systemRW = b
        }

    fun mount(): Boolean {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            return NativeAPI.mount()
        } else {
            return RootAPI.mount()
        }
    }

    fun isAppRequiredBySystem(pkgName: String?): Boolean {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            return NativeAPI.isAppRequiredBySystem(pkgName)
        } else {
            return RootAPI.isAppRequiredBySystem(pkgName)
        }
    }

    fun writeFile(ctx: Context, filePath: String, text: String, perm: Int): Boolean {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            return NativeAPI.writeFile(ctx, filePath, text, perm)
        } else {
            return RootAPI.writeFile(ctx, filePath, text, perm)
        }
    }

    fun catFile(src: String, dest: String, perm: Int): Boolean {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            return NativeAPI.catFile(src, dest, perm)
        } else {
            return RootAPI.catFile(src, dest, perm)
        }
    }

    fun systemClean(ctx: Context) {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.systemClean(ctx)
        } else {
            RootAPI.systemClean(ctx)
        }
    }

    fun freezeComponent(packageName: String, componentName: String, isFreezed: Boolean): Boolean {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            return NativeAPI.freezeComponent(packageName, componentName, isFreezed)
        } else {
            return RootAPI.freezeComponent(packageName, componentName, isFreezed)
        }
    }

    fun freezeComponents(packageName: String?, componentNames: Array<String>, isFreezed: Boolean): Boolean {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            return NativeAPI.freezeComponents(packageName, componentNames, isFreezed)
        } else {
            return RootAPI.freezeComponents(packageName, componentNames, isFreezed)
        }
    }

    fun freezeApplication(packageName: String?, isFreezed: Boolean): Boolean {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            return NativeAPI.freezeApplication(packageName, isFreezed)
        } else {
            return RootAPI.freezeApplication(packageName, isFreezed)
        }
    }

    fun deleteSystemApp(pkgName: String): Boolean {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            return NativeAPI.deleteSystemApp(pkgName)
        } else {
            return RootAPI.deleteSystemApp(pkgName)
        }
    }

    fun makePreferenceReadable(sdk: Int, packageName: String?) {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.makePreferenceReadable(sdk, packageName)
        } else {
            RootAPI.makePreferenceReadable(sdk, packageName)
        }
    }

    fun killProcess() {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.killProcess()
        } else {
            RootAPI.killProcess()
        }
    }

    fun forceDropCache() {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.forceDropCache()
        } else {
            RootAPI.forceDropCache()
        }
    }

    fun forceDeleteFile(path: String) {
        if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.forceDeleteFile(path)
        } else {
            RootAPI.forceDeleteFile(path)
        }
    }

    fun cleanCallback(ctx: Context, status: Int, data: String?) {
        val inCallback = Intent(CleanFragment.ACTION_CLEAN_CALLBACK)
        inCallback.putExtra(CleanFragment.KEY_STATUS, status)
        inCallback.putExtra(CleanFragment.KEY_DATA, data)
        ctx.sendBroadcast(inCallback)
    }
}
