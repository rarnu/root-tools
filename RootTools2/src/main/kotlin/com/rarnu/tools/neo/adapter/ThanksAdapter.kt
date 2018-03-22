package com.rarnu.tools.neo.adapter

import android.content.Context
import android.graphics.BitmapFactory
import android.view.View
import android.widget.ImageView
import android.widget.TextView
import com.rarnu.base.app.BaseAdapter
import com.rarnu.base.utils.DownloadUtils
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.data.ThanksInfo
import kotlinx.android.synthetic.main.listitem_thanks.view.*
import java.io.File

/**
 * Created by rarnu on 12/5/16.
 */
class ThanksAdapter(context: Context, list: MutableList<ThanksInfo?>?) : BaseAdapter<ThanksInfo?, ThanksAdapter.ThanksHolder>(context, list) {

    override fun fillHolder(baseVew: View, holder: ThanksHolder, item: ThanksInfo?) {
        holder.setItem(item)
    }

    override fun getAdapterLayout(): Int = R.layout.listitem_thanks

    override fun newHolder(baseView: View): ThanksHolder = ThanksHolder(baseView)

    override fun getValueText(item: ThanksInfo?): String? = ""
    
    inner class ThanksHolder(v: View) {
        private var ivHead: ImageView? = null
        private var tvName: TextView? = null
        private var tvDesc: TextView? = null

        internal fun setItem(item: ThanksInfo?) {
            // set head image
            if (item?.headFile == "") {
                ivHead?.setImageBitmap(null)
            } else {
                val filePath = context.getExternalFilesDir("").absolutePath + "/" + item?.headFile
                if (File(filePath).exists()) {
                    ivHead?.setImageBitmap(BitmapFactory.decodeFile(filePath))
                } else {
                    DownloadUtils.downloadFileT(context, ivHead, API.HEAD_URL + item!!.headFile, context.getExternalFilesDir("").absolutePath, item.headFile!!, null)
                }
            }
            tvName?.text = item?.name
            tvDesc?.text = if (RootApplication.isZh) item?.desc else item?.descEn
        }

        init {
            ivHead = v.ivHead
            tvName = v.tvName
            tvDesc = v.tvDesc
        }

    }

}