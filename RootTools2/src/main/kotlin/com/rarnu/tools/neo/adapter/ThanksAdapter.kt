package com.rarnu.tools.neo.adapter

import android.content.Context
import android.graphics.BitmapFactory
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.TextView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.base.BaseAdapter
import com.rarnu.tools.neo.data.ThanksInfo
import com.rarnu.tools.neo.utils.DownloadUtils
import java.io.File

/**
 * Created by rarnu on 12/5/16.
 */
class ThanksAdapter(context: Context, list: MutableList<ThanksInfo?>?) : BaseAdapter<ThanksInfo?>(context, list) {

    override fun getValueText(item: ThanksInfo?): String? = ""

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v: View? = convertView
        if (v == null) {
            v = inflater.inflate(R.layout.listitem_thanks, parent, false)
        }

        var holder: ThanksHolder? = v?.tag as ThanksHolder?
        if (holder ==  null) {
            holder = ThanksHolder(v, R.id.ivHead, R.id.tvName, R.id.tvDesc)
            v?.tag = holder
        }
        val item = list!![position]
        holder.setItem(item)
        return v
    }


    private inner class ThanksHolder {
        internal var ivHead: ImageView? = null
        internal var tvName: TextView? = null
        internal var tvDesc: TextView? = null

        constructor(v: View?, headId: Int, nameId: Int, descId: Int) {
            ivHead = v?.findViewById(headId) as ImageView?
            tvName = v?.findViewById(nameId) as TextView?
            tvDesc = v?.findViewById(descId) as TextView?
        }

        internal fun setItem(item: ThanksInfo?) {
            // set head image
            if (item?.headFile == "") {
                ivHead?.setImageBitmap(null)
            } else {
                val filePath = context.getExternalFilesDir("").absolutePath + "/" + item?.headFile
                if (File(filePath).exists()) {
                    ivHead?.setImageBitmap(BitmapFactory.decodeFile(filePath))
                } else {
                    DownloadUtils.startDownload(API.HEAD_URL + item?.headFile, filePath, this@ThanksAdapter)
                }
            }
            tvName?.text = item?.name
            tvDesc?.text = item?.desc
        }

    }


}