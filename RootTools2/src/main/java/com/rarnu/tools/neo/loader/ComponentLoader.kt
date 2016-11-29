package com.rarnu.tools.neo.loader

import android.content.Context
import android.content.pm.ApplicationInfo
import android.content.pm.PackageManager
import com.rarnu.tools.neo.base.BaseLoader
import com.rarnu.tools.neo.data.CompInfo
import com.rarnu.tools.neo.utils.ComponentUtils
import com.rarnu.tools.neo.utils.PackageParserUtils

/**
 * Created by rarnu on 9/2/16.
 */
class ComponentLoader(context: Context) : BaseLoader<CompInfo>(context) {

    private var type = 0
    private var pkg: String? = null

    override fun loadInBackground(): MutableList<CompInfo>? {
        var list: MutableList<CompInfo>? = null
        try {
            val info = context.packageManager.getApplicationInfo(pkg, 0)
            val ppu = PackageParserUtils()
            val obj = ppu.parsePackage(info.publicSourceDir, 0)
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
