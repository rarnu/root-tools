package com.rarnu.tools.neo.api

import android.content.Context
import android.content.Intent
import com.rarnu.tools.neo.fragment.CleanFragment
import com.rarnu.tools.neo.xposed.XpStatus

/**
 * Created by rarnu on 11/24/16.
 */
object DeviceAPI {

    const val STATUS_PROGRESS = 0
    const val STATUS_COMPLETE = 1
    const val STATUS_ERROR = 2

    var isRejected: Boolean
        get() = if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.rejected
        } else {
            RootAPI.rejected
        }
        set(b) = if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.rejected = b
        } else {
            RootAPI.rejected = b
        }

    var isSystemRW: Boolean
        get() = if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.isSystemRW()
        } else {
            RootAPI.isSystemRW()
        }
        set(b) = if (XpStatus.mode === XpStatus.Mode.NDK) {
            NativeAPI.systemRW = b
        } else {
            RootAPI.systemRW = b
        }

    fun mount(): Boolean = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.mount()
    } else {
        RootAPI.mount()
    }

    fun isAppRequiredBySystem(pkgName: String?): Boolean = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.isAppRequiredBySystem(pkgName)
    } else {
        RootAPI.isAppRequiredBySystem(pkgName)
    }

    fun writeFile(ctx: Context, filePath: String, text: String, perm: Int): Boolean = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.writeFile(ctx, filePath, text, perm)
    } else {
        RootAPI.writeFile(filePath, text, perm)
    }

    fun catFile(src: String?, dest: String?, perm: Int): Boolean = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.catFile(src, dest, perm)
    } else {
        RootAPI.catFile(src, dest, perm)
    }

    fun systemClean(ctx: Context?) = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.systemClean(ctx)
    } else {
        RootAPI.systemClean(ctx)
    }

    fun freezeComponent(packageName: String?, componentName: String?, isFreezed: Boolean): Boolean = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.freezeComponent(packageName, componentName, isFreezed)
    } else {
        RootAPI.freezeComponent(packageName, componentName, isFreezed)
    }

    fun freezeComponents(packageName: String?, componentNames: Array<String>?, isFreezed: Boolean): Boolean = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.freezeComponents(packageName, componentNames, isFreezed)
    } else {
        RootAPI.freezeComponents(packageName, componentNames, isFreezed)
    }

    fun freezeApplication(packageName: String?, isFreezed: Boolean): Boolean = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.freezeApplication(packageName, isFreezed)
    } else {
        RootAPI.freezeApplication(packageName, isFreezed)
    }

    fun deleteSystemApp(pkgName: String?): Boolean = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.deleteSystemApp(pkgName)
    } else {
        RootAPI.deleteSystemApp(pkgName)
    }

    fun makePreferenceReadable(sdk: Int, packageName: String?) = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.makePreferenceReadable(sdk, packageName)
    } else {
        RootAPI.makePreferenceReadable(sdk, packageName)
    }

    fun killProcess() = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.killProcess()
    } else {
        RootAPI.killProcess()
    }

    fun forceDropCache() = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.forceDropCache()
    } else {
        RootAPI.forceDropCache()
    }

    fun forceDeleteFile(path: String) = if (XpStatus.mode === XpStatus.Mode.NDK) {
        NativeAPI.forceDeleteFile(path)
    } else {
        RootAPI.forceDeleteFile(path)
    }

    fun cleanCallback(ctx: Context?, status: Int, data: String?) {
        val inCallback = Intent(CleanFragment.ACTION_CLEAN_CALLBACK)
        inCallback.putExtra(CleanFragment.KEY_STATUS, status)
        inCallback.putExtra(CleanFragment.KEY_DATA, data)
        ctx?.sendBroadcast(inCallback)
    }
}
