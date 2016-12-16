package com.rarnu.tools.neo.adapter

import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.Switch
import android.widget.TextView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.base.BaseAdapter
import com.rarnu.tools.neo.data.CompInfo

class CompDetailAdapter(context: Context, list: MutableList<CompInfo>?) : BaseAdapter<CompInfo>(context, list) {

    override fun getValueText(item: CompInfo): String? = item.compName

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v: View? = convertView
        if (v == null) {
            v = inflater.inflate(R.layout.listitem_compdetail, parent, false)
        }
        var holder: CompHolder? = v?.tag as CompHolder?
        if (holder == null) {
            holder = CompHolder(v, R.id.prefStatus, R.id.tvName, R.id.tvPackageName)
            v?.tag = holder
        }
        val item = list!![position]
        holder.setItem(item)
        return v
    }

    private inner class CompHolder {

        internal var prefStatus: Switch? = null
        internal var tvName: TextView? = null
        internal var tvPackageName: TextView? = null

        constructor(v: View?, statusId: Int, nameId: Int, pkgId: Int) {
            prefStatus = v?.findViewById(statusId) as Switch?
            tvName = v?.findViewById(nameId) as TextView?
            tvPackageName = v?.findViewById(pkgId) as TextView?
        }

        internal fun setItem(item: CompInfo) {
            prefStatus?.isChecked = item.enabled
            tvName?.text = item.compName
            tvPackageName?.text = item.fullPackageName?.substring(0, item.fullPackageName!!.lastIndexOf("."))
        }
    }
}
