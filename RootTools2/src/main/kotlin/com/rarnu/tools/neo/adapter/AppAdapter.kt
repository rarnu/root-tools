package com.rarnu.tools.neo.adapter

import android.content.Context
import android.view.View
import com.rarnu.kt.android.BaseAdapter
import com.rarnu.kt.android.attrColor
import com.rarnu.kt.android.resColor
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.data.AppInfo
import kotlinx.android.synthetic.main.listitem_app.view.*

class AppAdapter(context: Context, list: MutableList<AppInfo>) : BaseAdapter<AppInfo, AppAdapter.AppHolder>(context, list) {

    override fun fillHolder(baseVew: View, holder: AppHolder, item: AppInfo, position: Int) {
        holder.setItem(item)
        holder.prefStatus.visibility = if (showSwitch) View.VISIBLE else View.GONE
    }

    override fun getAdapterLayout() = R.layout.listitem_app

    override fun newHolder(baseView: View) = AppHolder(baseView)

    private var showSwitch = false

    override fun getValueText(item: AppInfo) = item.name + item.packageName

    fun setShowSwitch(on: Boolean) {
        showSwitch = on
        notifyDataSetChanged()
    }

    inner class AppHolder(v: View) {
        private var ivIcon = v.ivIcon
        internal var prefStatus = v.prefStatus
        private var tvName = v.tvName
        private var tvPackageName = v.tvPackageName

        internal fun setItem(item: AppInfo) {
            ivIcon.setImageDrawable(item.imageId)
            prefStatus.isChecked = !item.isDisable
            tvName.text = item.name
            tvPackageName.text = item.packageName
            if (item.isSystem) {
                tvName.setTextColor(context.resColor(android.R.color.holo_green_dark))
                tvPackageName.setTextColor(context.resColor(android.R.color.holo_green_light))
            } else {
                tvName.setTextColor(context.attrColor(android.R.attr.textColorPrimary))
                tvPackageName.setTextColor(context.attrColor(android.R.attr.textColorSecondary))
            }
        }

    }
}
