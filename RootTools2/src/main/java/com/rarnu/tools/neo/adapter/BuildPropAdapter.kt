package com.rarnu.tools.neo.adapter

import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.base.BaseAdapter
import com.rarnu.tools.neo.data.BuildPropInfo

class BuildPropAdapter(context: Context, list: MutableList<BuildPropInfo>) : BaseAdapter<BuildPropInfo>(context, list) {

    override fun getValueText(item: BuildPropInfo): String? = (item.buildName + item.buildValue)

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v: View? = convertView
        if (v == null) {
            v = inflater.inflate(R.layout.listitem_buildprop, parent, false)
        }
        var holder: BuildPropAdapterHolder? = v?.tag as BuildPropAdapterHolder?
        if (holder == null) {
            holder = BuildPropAdapterHolder(v, R.id.tvPropName, R.id.tvPropValue)
            v?.tag = holder
        }
        val item = list!![position]
        holder.setItem(item)
        return v
    }

    private inner class BuildPropAdapterHolder  {

        var tvPropName: TextView? = null
        var tvPropValue: TextView? = null

        constructor(v: View?, nameId: Int, valueId: Int) {
            tvPropName = v?.findViewById(nameId) as TextView?
            tvPropValue = v?.findViewById(valueId) as TextView?
        }

        internal fun setItem(item: BuildPropInfo) {
            tvPropName?.text = item.buildName
            tvPropValue?.text = item.buildValue
        }
    }

}
