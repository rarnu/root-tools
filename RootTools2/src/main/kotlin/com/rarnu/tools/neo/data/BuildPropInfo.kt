package com.rarnu.tools.neo.data

import com.rarnu.common.toPair
import java.io.Serializable

data class BuildPropInfo(var buildName: String, var buildValue: String) : Serializable {
    companion object {
        fun parse(str: String): BuildPropInfo = str.toPair().let { BuildPropInfo(it.first, it.second) }
    }
}
