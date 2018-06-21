package com.rarnu.tools.neo.loader

import android.content.Context
import com.rarnu.kt.android.BaseListLoader
import com.rarnu.tools.neo.data.BuildPropInfo
import com.rarnu.tools.neo.utils.BuildPropUtils

class BuildPropLoader(context: Context) : BaseListLoader<BuildPropInfo>(context) {

    override fun loadInBackground() = BuildPropUtils.buildProp
}
