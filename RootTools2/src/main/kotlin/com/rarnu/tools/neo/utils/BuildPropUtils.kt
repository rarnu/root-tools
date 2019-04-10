package com.rarnu.tools.neo.utils

import android.content.Context
import com.rarnu.kt.android.fileIO
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.data.BuildPropInfo

object BuildPropUtils {

    private const val PATH_BUILD_PROP = "/system/build.prop"

    val buildProp: MutableList<BuildPropInfo>
        get() {
            val list = mutableListOf<BuildPropInfo>()
            fileIO {
                src = PATH_BUILD_PROP
                isDestText = true
                result { _, text, _ ->
                    val file = text?.split("\n")
                    if (file != null && file.isNotEmpty()) {
                        file.filter { it.trim { i -> i <= ' ' } != "" && !it.trim { i -> i <= ' ' }.startsWith("#") && it.trim { i -> i <= ' ' }.contains("=") }
                                .mapTo(list) { BuildPropInfo.parse(it) }
                    }
                }
            }
            return list
        }

    fun setBuildProp(ctx: Context, list: List<BuildPropInfo>): Boolean {
        var ret = false
        var str = ""
        for ((buildName, buildValue) in list) {
            str += "$buildName=$buildValue\n"
        }
        try {
            DeviceAPI.mount()
            ret = DeviceAPI.writeFile(ctx, PATH_BUILD_PROP, str, 755)
        } catch (e: Exception) {

        }

        return ret
    }
}
