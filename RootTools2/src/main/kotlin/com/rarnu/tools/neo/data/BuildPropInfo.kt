package com.rarnu.tools.neo.data

import java.io.Serializable

data class BuildPropInfo(
        var buildName: String?,
        var buildValue: String?
) : Serializable {

    companion object {
        fun parse(str: String?): BuildPropInfo =
                BuildPropInfo(
                        str?.substring(0, str.indexOf("="))?.trim { it <= ' ' },
                        str?.substring(str.indexOf("=") + 1)?.trim { it <= ' ' }
                )
    }
}
