package com.rarnu.tools.neo.adapter

import android.content.Context
import android.graphics.BitmapFactory
import android.view.View
import android.widget.ImageView
import android.widget.TextView
import com.rarnu.kt.android.BaseAdapter
import com.rarnu.kt.android.DownloadState
import com.rarnu.kt.android.downloadAsync
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.data.ThanksInfo
import kotlinx.android.synthetic.main.listitem_thanks.view.*
import java.io.File

/**
 * Created by rarnu on 12/5/16.
 */
class ThanksAdapter(context: Context, list: MutableList<ThanksInfo>) : BaseAdapter<ThanksInfo, ThanksAdapter.ThanksHolder>(context, list) {

    override fun fillHolder(baseVew: View, holder: ThanksHolder, item: ThanksInfo) {
        holder.setItem(item)
    }

    override fun getAdapterLayout() = R.layout.listitem_thanks

    override fun newHolder(baseView: View) = ThanksHolder(baseView)

    override fun getValueText(item: ThanksInfo) = ""

    inner class ThanksHolder(v: View) {
        private var ivHead: ImageView = v.ivHead
        private var tvName: TextView = v.tvName
        private var tvDesc: TextView = v.tvDesc

        internal fun setItem(item: ThanksInfo?) {
            // set head image
            if (item?.headFile == "") {
                ivHead.setImageBitmap(null)
            } else {
                val filePath = context.getExternalFilesDir("").absolutePath + "/" + item?.headFile
                if (File(filePath).exists()) {
                    ivHead.setImageBitmap(BitmapFactory.decodeFile(filePath))
                } else {
                    if (item != null) {
                        downloadAsync {
                            url = API.HEAD_URL + item.headFile
                            localFile = File(context.getExternalFilesDir(""), item.headFile).absolutePath
                            progress { state, _, _, _ ->
                                if (state == DownloadState.WHAT_DOWNLOAD_FINISH) {
                                    ivHead.setImageBitmap(BitmapFactory.decodeFile(localFile))
                                }
                            }
                        }
                    }
                }
            }
            tvName.text = item?.name
            tvDesc.text = if (RootApplication.isZh) item?.desc else item?.descEn
        }

    }

}