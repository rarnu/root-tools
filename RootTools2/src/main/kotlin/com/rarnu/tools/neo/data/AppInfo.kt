package com.rarnu.tools.neo.data

import android.graphics.drawable.Drawable

data class AppInfo(
        var name: String?,
        var imageId: Drawable?,
        var packageName: String?,
        var isDisable: Boolean,
        var version: String?,
        var versionCode: Int,
        var isSystem: Boolean,
        var isForFreeze: Boolean
)