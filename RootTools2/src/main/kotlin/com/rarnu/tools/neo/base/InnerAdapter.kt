package com.rarnu.tools.neo.base

import android.content.Context
import android.content.pm.PackageManager
import android.view.LayoutInflater
import android.widget.BaseAdapter
import android.widget.Filter
import android.widget.Filterable

/**
 * Created by rarnu on 3/23/16.
 */
abstract class InnerAdapter<T> : BaseAdapter, Filterable {

    protected val lock = Any()
    protected var context: Context
    protected var inflater: LayoutInflater
    protected var listFull: MutableList<T>?
    protected var list: MutableList<T>?
    private  var _filter: ArrayFilter? = null
    protected var pm: PackageManager

    constructor(context: Context, list: MutableList<T>?): super() {
        this.context = context
        this.inflater = LayoutInflater.from(context)
        this.listFull = list
        this.list = list
        this.pm = context.packageManager
    }

    open fun setNewList(list: MutableList<T>?) {
        this.listFull = list
        this.list = list
        notifyDataSetChanged()
    }

    open fun deleteItem(item: T) {
        list?.remove(item)
        listFull?.remove(item)
        notifyDataSetChanged()
    }

    open fun deleteItems(items: MutableList<T>) {
        for (i in items) {
            list?.remove(i)
            listFull?.remove(i)
        }
        notifyDataSetChanged()
    }

    override fun getFilter(): Filter? {
        if (_filter == null) {
            _filter = ArrayFilter()
        }
        return _filter
    }

    override fun getItem(position: Int): Any? = list!![position]

    open fun getFiltedItem(position: Int): T = list!![position]

    override fun getItemId(position: Int): Long = position.toLong()

    override fun getCount(): Int = list!!.size

    abstract fun getValueText(item: T): String?

    open fun filter(text: String?) = filter?.filter(text)

    inner class ArrayFilter: Filter() {
        override fun performFiltering(prefix: CharSequence?): FilterResults? {
            list = listFull
            val results = FilterResults()
            if (prefix == null || prefix.isEmpty()) {
                synchronized (lock) {
                    val l = list
                    results.values = l
                    results.count = l!!.size
                }
            } else {
                val prefixString = prefix.toString().toLowerCase()
                val values = list
                val count = values!!.size
                val newValues = arrayListOf<T>()

                (0..count - 1).forEach {
                    val value = values[it]
                    val valueText = getValueText(value)?.toLowerCase()
                    if (valueText?.indexOf(prefixString) != -1) {
                        newValues.add(value)
                    }
                }
                results.values = newValues
                results.count = newValues.size
            }

            return results
        }

        @Suppress("UNCHECKED_CAST")
        override fun publishResults(constraint: CharSequence?, results: FilterResults?) {
            if (results != null) {
                list = results.values as MutableList<T>?
                if (results.count > 0) {
                    notifyDataSetChanged()
                } else {
                    notifyDataSetInvalidated()
                }
            }
        }

    }
}