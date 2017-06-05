package com.rarnu.tools.neo.loader

import android.content.Context
import com.rarnu.base.app.BaseLoader
import com.rarnu.tools.neo.data.BuildPropInfo
import com.rarnu.tools.neo.utils.BuildPropUtils

class BuildPropLoader(context: Context) : BaseLoader<BuildPropInfo>(context) {

    override fun loadInBackground(): MutableList<BuildPropInfo>? = BuildPropUtils.buildProp
}
