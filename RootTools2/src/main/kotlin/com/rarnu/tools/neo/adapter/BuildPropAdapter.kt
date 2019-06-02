package com.rarnu.tools.neo.adapter

import android.content.Context
import android.view.View
import com.rarnu.android.BaseAdapter
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.data.BuildPropInfo
import kotlinx.android.synthetic.main.listitem_buildprop.view.*

class BuildPropAdapter(context: Context, list: MutableList<BuildPropInfo>) : BaseAdapter<BuildPropInfo, BuildPropAdapter.BuildPropAdapterHolder>(context, list) {

    override fun fillHolder(baseVew: View, holder: BuildPropAdapterHolder, item: BuildPropInfo, position: Int) {
        holder.setItem(item)
    }

    override fun getAdapterLayout() = R.layout.listitem_buildprop

    override fun newHolder(baseView: View) = BuildPropAdapterHolder(baseView)

    override fun getValueText(item: BuildPropInfo) = item.buildName + item.buildValue

    inner class BuildPropAdapterHolder(v: View) {

        private var tvPropName = v.tvPropName
        private var tvPropValue = v.tvPropValue

        internal fun setItem(item: BuildPropInfo) {
            tvPropName.text = item.buildName
            tvPropValue.text = item.buildValue
        }
    }

}
