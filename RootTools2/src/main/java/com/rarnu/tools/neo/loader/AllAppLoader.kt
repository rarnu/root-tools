package com.rarnu.tools.neo.loader

import android.content.Context
import com.rarnu.tools.neo.base.BaseLoader
import com.rarnu.tools.neo.data.AppInfo
import com.rarnu.tools.neo.utils.AppUtils

class AllAppLoader(context: Context) : BaseLoader<AppInfo>(context) {
    override fun loadInBackground(): MutableList<AppInfo>? = AppUtils.getInstalledApps(context)
}
