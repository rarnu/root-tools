@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.Manifest
import android.content.Intent
import android.content.SharedPreferences
import android.content.pm.PackageManager
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.os.Build
import android.os.Bundle
import android.preference.PreferenceManager
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.ImageView
import android.widget.RelativeLayout
import android.widget.TextView
import com.rarnu.kt.android.BackActivity
import com.rarnu.kt.android.resStr
import com.rarnu.kt.android.save
import com.rarnu.kt.android.toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.xposed.XpStatus
import kotlinx.android.synthetic.main.fragment_feedback.*
import java.util.*
import kotlin.concurrent.thread

/**
 * Created by rarnu on 11/19/16.
 */
class FeedbackActivity : BackActivity(), View.OnClickListener {

    companion object {
        private const val KEY_NICKNAME = "__nickname"
    }

    private lateinit var miSend: MenuItem
    private val ph = arrayOfNulls<RelativeLayout>(5)
    private val tvAdd = arrayOfNulls<TextView>(5)
    private val imgP = arrayOfNulls<ImageView>(5)
    private lateinit var sp: SharedPreferences
    private val path = arrayOf("", "", "", "", "")

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_feedback)
        actionBar?.title = resStr(R.string.about_feedback)
        (0..4).forEach {
            ph[it] = findViewById(resources.getIdentifier("ph${it + 1}", "id", packageName))
            ph[it]?.setOnClickListener(this)
            imgP[it] = findViewById(resources.getIdentifier("imgP${it + 1}", "id", packageName))
            tvAdd[it] = findViewById(resources.getIdentifier("tvAdd${it + 1}", "id", packageName))
        }

        sp = PreferenceManager.getDefaultSharedPreferences(this)
        etNickname.setText(sp.getString(KEY_NICKNAME, ""))

        if (Build.VERSION.SDK_INT >= 23) {
            if (checkSelfPermission(Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(arrayOf(Manifest.permission.WRITE_EXTERNAL_STORAGE, Manifest.permission.READ_EXTERNAL_STORAGE), 0)
            } else {
                XpStatus.canWriteSdcard = true
            }
        } else {
            XpStatus.canWriteSdcard = true
        }
    }

    override fun onRequestPermissionsResult(requestCode: Int, permissions: Array<String>, grantResults: IntArray) {
        if (Build.VERSION.SDK_INT >= 23) {
            super.onRequestPermissionsResult(requestCode, permissions, grantResults)
            permissions.indices.forEach {
                if (permissions[it] == Manifest.permission.WRITE_EXTERNAL_STORAGE) {
                    XpStatus.canWriteSdcard = grantResults[it] == PackageManager.PERMISSION_GRANTED
                }
            }
        }
    }

    override fun onClick(v: View) {
        val id = v.id
        for (i in 0..4) {
            if (id == ph[i]?.id) {
                chooseScreenshot(i)
                break
            }
        }
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        miSend = menu.add(0, 1, 1, R.string.ab_send)
        miSend.setIcon(android.R.drawable.ic_menu_send)
        miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        return super.onCreateOptionsMenu(menu)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            1 -> sendFeedback()
        }
        return super.onOptionsItemSelected(item)
    }

    private fun sendFeedback() {
        val nickname = etNickname.text.toString()
        if (nickname == "") {
            toast(resStr(R.string.toast_nickname_empty))
            return
        }
        val comment = etComment.text.toString()
        if (comment == "") {
            toast(resStr(R.string.toast_comment_empty))
            return
        }
        miSend.isEnabled = false
        sp.edit().putString(KEY_NICKNAME, etNickname.text.toString()).apply()
        thread {
            val ret = API.sendFeedback(nickname, comment, path)
            runOnUiThread {
                miSend.isEnabled = true
                if (ret) {
                    toast(resStr(R.string.toast_send_feedback_ok))
                    finish()
                } else {
                    toast(resStr(R.string.toast_send_feedback_fail))
                }
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
        if (resultCode == RESULT_OK) {
            try {
                val uri = data?.data
                val bop = BitmapFactory.Options()
                bop.inSampleSize = 2
                val bmp = BitmapFactory.decodeStream(contentResolver.openInputStream(uri!!), null, bop)
                val filePath = generateLocalFileName()
                path[requestCode] = generateFullPath(filePath)
                bmp?.save(path[requestCode], Bitmap.CompressFormat.PNG)
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
        val fFile = getExternalFilesDir("")
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

    private fun generateLocalFileName() = UUID.randomUUID().toString() + ".png"

}
