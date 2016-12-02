package com.rarnu.tools.neo.fragment

import android.app.Activity
import android.content.Intent
import android.content.SharedPreferences
import android.graphics.BitmapFactory
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.preference.PreferenceManager
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.ImageView
import android.widget.RelativeLayout
import android.widget.TextView
import android.widget.Toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.base.BaseFragment
import com.rarnu.tools.neo.utils.ImageUtils
import java.util.*
import kotlin.concurrent.thread

/**
 * Created by rarnu on 11/19/16.
 */
class FeedbackFragment : BaseFragment(), View.OnClickListener {

    private var miSend: MenuItem? = null
    private var etNickname: TextView? = null
    private var etComment: TextView? = null
    private val ph = arrayOfNulls<RelativeLayout>(5)
    private val tvAdd = arrayOfNulls<TextView>(5)
    private val imgP = arrayOfNulls<ImageView>(5)
    private var sp: SharedPreferences? = null
    private val path = arrayOf("", "", "", "", "")

    override fun getBarTitle(): Int = R.string.about_feedback

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        etNickname = innerView?.findViewById(R.id.etNickname) as TextView?
        etComment = innerView?.findViewById(R.id.etComment) as TextView?
        for (i in 0..4) {
            ph[i] = innerView?.findViewById(resources.getIdentifier("ph${i + 1}", "id", context.packageName)) as RelativeLayout?
            imgP[i] = innerView?.findViewById(resources.getIdentifier("imgP${i + 1}", "id", context.packageName)) as ImageView?
            tvAdd[i] = innerView?.findViewById(resources.getIdentifier("tvAdd${i + 1}", "id", context.packageName)) as TextView?
        }
        sp = PreferenceManager.getDefaultSharedPreferences(context)
    }

    override fun initEvents() {
        for (i in 0..4) {
            ph[i]?.setOnClickListener(this)
        }
    }

    override fun initLogic() {
        // load nickname cache
        etNickname?.text = sp?.getString(KEY_NICKNAME, "")
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_feedback

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) {
        menu?.clear()
        miSend = menu?.add(0, 1, 1, R.string.ab_send)
        miSend?.setIcon(android.R.drawable.ic_menu_send)
        miSend?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            1 -> sendFeedback()
        }
        return true
    }

    private val hFeedback = object : Handler() {
        override fun handleMessage(msg: Message) {
            miSend?.isEnabled = true
            if (msg.what == 0) {
                Toast.makeText(context, R.string.toast_send_feedback_ok, Toast.LENGTH_SHORT).show()
                activity.finish()
            } else {
                Toast.makeText(context, R.string.toast_send_feedback_fail, Toast.LENGTH_SHORT).show()
            }
            super.handleMessage(msg)
        }
    }

    private fun sendFeedback() {
        val nickname = etNickname?.text.toString()
        if (nickname == "") {
            Toast.makeText(context, R.string.toast_nickname_empty, Toast.LENGTH_SHORT).show()
            return
        }
        val comment = etComment?.text.toString()
        if (comment == "") {
            Toast.makeText(context, R.string.toast_comment_empty, Toast.LENGTH_SHORT).show()
            return
        }
        miSend?.isEnabled = false
        sp?.edit()?.putString(KEY_NICKNAME, etNickname?.text.toString())?.apply()
        thread {
            // send feedback async
            val ret = API.sendFeedback(nickname, comment, path)
            hFeedback.sendEmptyMessage(if (ret) 0 else 1)
        }
    }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View) {
        // upload screenshot
        val id = v.id
        for (i in 0..4) {
            if (id == ph[i]?.id) {
                chooseScreenshot(i)
                break
            }
        }
    }

    private fun chooseScreenshot(i: Int) {
        val intent = Intent(Intent.ACTION_PICK)
        intent.type = "image/*"
        startActivityForResult(intent, i)
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        // album callback data
        if (resultCode == Activity.RESULT_OK) {
            try {
                val uri = data?.data
                val bop = BitmapFactory.Options()
                bop.inSampleSize = 2
                val bmp = BitmapFactory.decodeStream(context.contentResolver.openInputStream(uri), null, bop)
                val filePath = generateLocalFileName()
                path[requestCode] = generateFullPath(filePath)
                ImageUtils.saveBitmapToFile(bmp, path[requestCode])
                imgP[requestCode]?.setImageBitmap(bmp)
                tvAdd[requestCode]?.visibility = View.GONE
            } catch (e: Exception) {

            }

        } else {
            try {
                imgP[requestCode]?.setImageBitmap(null)
                path[requestCode] = ""
                tvAdd[requestCode]?.visibility = View.VISIBLE
            } catch (e: Exception) {

            }
        }
    }

    private fun generateFullPath(uuid: String): String {
        var ret = ""
        val fFile = context.getExternalFilesDir("")
        if (fFile != null) {
            if (!fFile.exists()) {
                fFile.mkdir()
            }
            var cachePath = fFile.absolutePath
            if (!cachePath.endsWith("/")) {
                cachePath += "/"
            }
            ret = cachePath + uuid
        }
        return ret
    }

    private fun generateLocalFileName(): String = UUID.randomUUID().toString() + ".png"

    companion object {
        private val KEY_NICKNAME = "__nickname"
    }


}
