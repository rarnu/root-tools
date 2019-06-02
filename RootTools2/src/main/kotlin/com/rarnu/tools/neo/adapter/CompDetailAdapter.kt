package com.rarnu.tools.neo.adapter

import android.content.Context
import android.view.View
import com.rarnu.android.BaseAdapter
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.utils.CompInfo
import kotlinx.android.synthetic.main.listitem_compdetail.view.*

class CompDetailAdapter(context: Context, list: MutableList<CompInfo>) : BaseAdapter<CompInfo, CompDetailAdapter.CompHolder>(context, list) {

    override fun fillHolder(baseVew: View, holder: CompHolder, item: CompInfo, position: Int) {
        holder.setItem(item)
    }

    override fun getAdapterLayout() = R.layout.listitem_compdetail

    override fun newHolder(baseView: View) = CompHolder(baseView)

    override fun getValueText(item: CompInfo) = item.pureName

    inner class CompHolder(v: View) {

        private var prefStatus = v.prefStatus
        private var tvName = v.tvName
        private var tvPackageName = v.tvPackageName

        internal fun setItem(item: CompInfo) {
            prefStatus.isChecked = item.enabled
            tvName.text = item.pureName
            tvPackageName.text = item.componentClassName
        }

    }
}
