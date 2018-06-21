package com.rarnu.tools.neo.adapter

import android.content.Context
import android.view.View
import android.widget.Switch
import android.widget.TextView
import com.rarnu.kt.android.BaseAdapter
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.utils.CompInfo
import kotlinx.android.synthetic.main.listitem_compdetail.view.*

class CompDetailAdapter(context: Context, list: MutableList<CompInfo>) : BaseAdapter<CompInfo, CompDetailAdapter.CompHolder>(context, list) {

    override fun fillHolder(baseVew: View, holder: CompHolder, item: CompInfo) {
        holder.setItem(item)
    }

    override fun getAdapterLayout() = R.layout.listitem_compdetail

    override fun newHolder(baseView: View) = CompHolder(baseView)

    override fun getValueText(item: CompInfo) = item.compName

    inner class CompHolder(v: View) {

        private var prefStatus: Switch = v.prefStatus
        private var tvName: TextView = v.tvName
        private var tvPackageName: TextView = v.tvPackageName

        internal fun setItem(item: CompInfo) {
            prefStatus.isChecked = item.enabled
            tvName.text = item.compName
            tvPackageName.text = item.fullPackageName?.substring(0, item.fullPackageName!!.lastIndexOf("."))
        }

    }
}
