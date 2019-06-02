package com.rarnu.tools.neo.loader

import android.content.Context
import android.content.pm.PackageManager
import com.rarnu.android.BaseListLoader
import com.rarnu.tools.neo.utils.CompInfo
import com.rarnu.tools.neo.utils.ComponentUtils

/**
 * Created by rarnu on 9/2/16.
 */
class ComponentLoader(context: Context) : BaseListLoader<CompInfo>(context) {

    private var type = 0
    private var pkg: String? = null

    override fun loadInBackground(): List<CompInfo> {
        val list = mutableListOf<CompInfo>()
        try {
            val info = context.packageManager.getPackageInfo(pkg, PackageManager.GET_ACTIVITIES or PackageManager.GET_SERVICES or PackageManager.GET_RECEIVERS or PackageManager.GET_PROVIDERS)
            when (type) {
                0 -> list.addAll(ComponentUtils.getActivityList(context, info))
                1 -> list.addAll(ComponentUtils.getServiceList(context, info))
                2 -> list.addAll(ComponentUtils.getReceiverList(context, info))
                3 -> list.addAll(ComponentUtils.getProviderList(context, info))
            }
        } catch (e: Exception) {
        }
        return list
    }

    fun startLoading(pkg: String?, type: Int) {
        this.type = type
        this.pkg = pkg
        startLoading()
    }
}
