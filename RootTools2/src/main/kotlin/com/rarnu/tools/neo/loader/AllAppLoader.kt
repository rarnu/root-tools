package com.rarnu.tools.neo.loader

import android.content.Context
import com.rarnu.kt.android.BaseListLoader
import com.rarnu.tools.neo.data.AppInfo
import com.rarnu.tools.neo.utils.AppUtils

class AllAppLoader(context: Context) : BaseListLoader<AppInfo>(context) {
    override fun loadInBackground() = AppUtils.getInstalledApps(context)
}
