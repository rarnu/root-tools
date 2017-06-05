package com.rarnu.tools.neo.adapter

import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import com.rarnu.base.app.BaseAdapter
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.data.BuildPropInfo
import kotlinx.android.synthetic.main.listitem_buildprop.view.*

class BuildPropAdapter(context: Context, list: MutableList<BuildPropInfo>?) : BaseAdapter<BuildPropInfo, BuildPropAdapter.BuildPropAdapterHolder>(context, list) {

    override fun fillHolder(baseVew: View, holder: BuildPropAdapterHolder, item: BuildPropInfo) {
        holder.setItem(item)
    }

    override fun getAdapterLayout(): Int = R.layout.listitem_buildprop

    override fun newHolder(baseView: View): BuildPropAdapterHolder = BuildPropAdapterHolder(baseView)

    override fun getValueText(item: BuildPropInfo): String? = (item.buildName + item.buildValue)

    inner class BuildPropAdapterHolder  {

        var tvPropName: TextView? = null
        var tvPropValue: TextView? = null

        constructor(v: View) {
            tvPropName = v.tvPropName
            tvPropValue = v.tvPropValue
        }

        internal fun setItem(item: BuildPropInfo) {
            tvPropName?.text = item.buildName
            tvPropValue?.text = item.buildValue
        }
    }

}
