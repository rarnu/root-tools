package com.rarnu.tools.neo.adapter

import android.content.Context
import android.graphics.Color
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.Switch
import android.widget.TextView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.base.BaseAdapter
import com.rarnu.tools.neo.data.AppInfo

class AppAdapter(context: Context, list: MutableList<AppInfo>?) : BaseAdapter<AppInfo>(context, list) {

    private var showSwitch = false

    override fun getValueText(item: AppInfo): String? = (item.name + item.packageName)

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v: View? = convertView
        if (v == null) {
            v = inflater.inflate(R.layout.listitem_app, parent, false)
        }
        var holder: AppHolder? = v?.tag as AppHolder?
        if (holder == null) {
            holder = AppHolder(v, R.id.ivIcon, R.id.prefStatus, R.id.tvName, R.id.tvPackageName)
            v?.tag = holder
        }
        val item = list!![position]
        holder.setItem(item)
        holder.prefStatus?.visibility = if (showSwitch) View.VISIBLE else View.GONE
        return v
    }

    fun setShowSwitch(on: Boolean) {
        showSwitch = on
        notifyDataSetChanged()
    }

    private inner class AppHolder {
        internal var ivIcon: ImageView? = null
        internal var prefStatus: Switch? = null
        internal var tvName: TextView? = null
        internal var tvPackageName: TextView? = null

        constructor(v: View?, iconId: Int, statusId: Int, nameId: Int, pkgId: Int) {
            ivIcon = v?.findViewById(iconId) as ImageView?
            prefStatus = v?.findViewById(statusId) as Switch?
            tvName = v?.findViewById(nameId) as TextView?
            tvPackageName = v?.findViewById(pkgId) as TextView?
        }

        internal fun setItem(item: AppInfo) {
            ivIcon?.setImageDrawable(item.imageId)
            prefStatus?.isChecked = !item.isDisable
            tvName?.text = item.name
            tvPackageName?.text = item.packageName
            if (item.isSystem) {
                tvName?.setTextColor(context.resources.getColor(android.R.color.holo_green_dark))
                tvPackageName?.setTextColor(context.resources.getColor(android.R.color.holo_green_light))
            } else {
                tvName?.setTextColor(Color.BLACK)
                tvPackageName?.setTextColor(Color.DKGRAY)
            }
        }
    }
}
