package com.rarnu.mi8

import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.BaseAdapter
import android.widget.TextView

/**
 * Created by rarnu on 6/27/16.
 */
class UserAdapter: BaseAdapter {


    private var _context: Context? = null
    private var _idList: MutableList<String>? = null
    private var _pathList: MutableList<String>? = null
    private var _inflater: LayoutInflater? = null

    constructor(context: Context, idList: MutableList<String>?, pathList: MutableList<String>?): super() {
        _context = context
        _idList = idList
        _pathList = pathList
        _inflater = LayoutInflater.from(_context)
    }

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v: View? = convertView
        if (v == null) {
            v = _inflater?.inflate(R.layout.item_user, parent, false)
        }
        var holder = v?.tag as UserHolder?
        if (holder == null) {
            holder = UserHolder()
            holder.tvUserId = v?.findViewById(R.id.tvUserId) as TextView
            holder.tvUserPath = v?.findViewById(R.id.tvUserPath) as TextView
            v?.tag = holder
        }
        holder.tvUserId?.text = _idList!![position]
        holder.tvUserPath?.text = _pathList!![position]
        holder.tvUserId?.setTextColor(_context!!.resources.getColor(if (position == 0) R.color.orange else R.color.black, _context!!.theme))
        holder.tvUserPath?.setTextColor(_context!!.resources.getColor(if (position == 0) R.color.lightorange else R.color.gray, _context!!.theme))
        return v
    }

    override fun getItem(position: Int): Any? = if (_idList != null ) { _idList!![position] } else { null }

    override fun getItemId(position: Int): Long = position.toLong()

    override fun getCount(): Int = if (_idList != null) { _idList!!.size } else { 0 }

    class UserHolder {
        var tvUserId: TextView? = null
        var tvUserPath: TextView? = null
    }
}