package com.rarnu.tools.neo.utils

import android.content.Context
import com.rarnu.common.asFileReadText
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.data.BuildPropInfo

object BuildPropUtils {

    private const val PATH_BUILD_PROP = "/system/build.prop"

    val buildProp: MutableList<BuildPropInfo>
        get() {
            val list = mutableListOf<BuildPropInfo>()
            val text = try { PATH_BUILD_PROP.asFileReadText() } catch (e: Throwable) { null }
            val file = text?.split("\n")
            if (file != null && file.isNotEmpty()) {
                file.filter { it.trim() != "" && !it.trim().startsWith("#") && it.trim().contains("=") }.mapTo(list) { BuildPropInfo.parse(it) }
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
