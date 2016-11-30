package com.rarnu.tools.neo.utils


import android.content.Context
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.data.BuildPropInfo

object BuildPropUtils {

    private val PATH_BUILD_PROP = "/system/build.prop"

    val buildProp: MutableList<BuildPropInfo>?
        get() {
            var list: MutableList<BuildPropInfo>? = null
            try {
                val file = FileUtils.readFile(PATH_BUILD_PROP)
                if (file != null && file.size != 0) {
                    list = arrayListOf<BuildPropInfo>()
                    for (f in file) {
                        if (f.trim { it <= ' ' } == "" || f.trim { it <= ' ' }.startsWith("#") || !f.trim { it <= ' ' }.contains("=")) {
                            continue
                        }
                        list.add(BuildPropInfo.parse(f))
                    }
                }
            } catch (e: Exception) {

            }
            return list
        }

    fun setBuildProp(ctx: Context?, list: List<BuildPropInfo>?): Boolean {
        var ret = false
        var str = ""
        for ((buildName, buildValue) in list!!) {
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
