package com.rarnu.tools.neo.loader

import android.content.Context
import com.rarnu.base.app.BaseLoader
import com.rarnu.base.utils.ComponentUtils
import com.rarnu.base.utils.PackageParserP
import com.rarnu.base.utils.parsePackage

/**
 * Created by rarnu on 9/2/16.
 */
class ComponentLoader(context: Context) : BaseLoader<ComponentUtils.CompInfo>(context) {

    private var type = 0
    private var pkg: String? = null

    override fun loadInBackground(): MutableList<ComponentUtils.CompInfo>? {
        var list: MutableList<ComponentUtils.CompInfo>? = null
        try {
            val info = context.packageManager.getApplicationInfo(pkg, 0)
            val ppu = PackageParserP.newPackageParser()
            val obj = ppu?.parsePackage(info.publicSourceDir, 0)
            when (type) {
                0 -> list = ComponentUtils.getActivityList(context, obj)
                1 -> list = ComponentUtils.getServiceList(context, obj)
                2 -> list = ComponentUtils.getReceiverList(context, obj)
                3 -> list = ComponentUtils.getProviderList(context, obj)
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
