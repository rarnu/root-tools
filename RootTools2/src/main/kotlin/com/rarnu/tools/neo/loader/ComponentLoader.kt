package com.rarnu.tools.neo.loader

import android.content.Context
import com.rarnu.kt.android.BaseListLoader
import com.rarnu.kt.android.PackageParserP
import com.rarnu.kt.android.parsePackage
import com.rarnu.tools.neo.utils.CompInfo
import com.rarnu.tools.neo.utils.ComponentUtils

/**
 * Created by rarnu on 9/2/16.
 */
class ComponentLoader(context: Context) : BaseListLoader<CompInfo>(context) {

    private var type = 0
    private var pkg: String? = null

    override fun loadInBackground(): MutableList<CompInfo>? {
        val list = arrayListOf<CompInfo>()
        try {
            val info = context.packageManager.getApplicationInfo(pkg, 0)
            val ppu = PackageParserP.newPackageParser()
            val obj = ppu?.parsePackage(info.publicSourceDir, 0)
            when (type) {
                0 -> list.addAll(ComponentUtils.getActivityList(context, obj))
                1 -> list.addAll(ComponentUtils.getServiceList(context, obj))
                2 -> list.addAll(ComponentUtils.getReceiverList(context, obj))
                3 -> list.addAll(ComponentUtils.getProviderList(context, obj))
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
