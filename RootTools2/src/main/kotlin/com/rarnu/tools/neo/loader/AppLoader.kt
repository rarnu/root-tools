package com.rarnu.tools.neo.loader

import android.content.Context
import com.rarnu.android.BaseListLoader
import com.rarnu.tools.neo.data.AppInfo
import com.rarnu.tools.neo.utils.AppUtils

class AppLoader(context: Context) : BaseListLoader<AppInfo>(context) {
    override fun loadInBackground() = AppUtils.getSystemApps(context)

}
