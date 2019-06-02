@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.Build
import android.os.Bundle
import android.os.Environment
import android.view.KeyEvent
import android.view.Menu
import android.view.MenuItem
import android.widget.ScrollView
import com.rarnu.android.*
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.DeviceAPI
import kotlinx.android.synthetic.main.fragment_clean.*
import java.io.File
import kotlin.concurrent.thread

class CleanActivity : BackActivity() {

    companion object {
        const val ACTION_CLEAN_CALLBACK = "com.rarnu.tools.neo.clean.callback"
        const val KEY_STATUS = "status"
        const val KEY_DATA = "data"
    }

    private lateinit var miRun: MenuItem
    private var isCleaning = false

    private val filterCallback = IntentFilter(ACTION_CLEAN_CALLBACK)
    private val receiverCallback = CleanCallbackReceiver()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_clean)
        actionBar?.title = resStr(R.string.clean_name)
        registerReceiver(receiverCallback, filterCallback)

        val busyboxExists = File("/system/bin/busybox").exists() || File("/system/xbin/busybox").exists()
        val duExists = File("/system/bin/du").exists() || File("/system/xbin/du").exists()
        if (duExists) {
            tvClean.setText(R.string.view_ready)
            return
        }
        if (busyboxExists) {
            tvClean.setText(R.string.view_ready)
            return
        }
        tvClean.setText(R.string.view_not_ready)
        threadExtractBusybox()
    }

    private fun threadExtractBusybox() = thread {
        // extract busybox
        val abis = Build.SUPPORTED_ABIS
        var busyboxAsset = "busybox_arm"
        for (abi in abis) {
            if (abi.toLowerCase().contains("mips")) {
                busyboxAsset = "busybox_mips"
                break
            }
            if (abi.toLowerCase().contains("x86")) {
                busyboxAsset = "busybox_x86"
                break
            }
        }
        val tmpDir = Environment.getExternalStorageDirectory().absolutePath
        val fDir = File(tmpDir, ".tmp")
        if (!fDir.exists()) {
            fDir.mkdirs()
        }
        val fBusybox = File(fDir, busyboxAsset)
        assetsIO {
            src = busyboxAsset
            dest = File(fDir, busyboxAsset)
        }
        DeviceAPI.mount()
        val ret = DeviceAPI.catFile(fBusybox.absolutePath, "/system/xbin/busybox", 755)
        runOnMainThread {
            if (ret) {
                tvClean.setText(R.string.view_ready)
            } else {
                tvClean.setText(R.string.view_env_error)
                miRun.isEnabled = false
            }
        }
    }


    override fun onDestroy() {
        unregisterReceiver(receiverCallback)
        super.onDestroy()
    }


    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        miRun = menu.add(0, 1, 1, R.string.ab_clean)
        miRun.setIcon(android.R.drawable.ic_menu_delete)
        miRun.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        return super.onCreateOptionsMenu(menu)
    }

    override fun onKeyDown(keyCode: Int, event: KeyEvent): Boolean {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (isCleaning) {
                toast(resStr(R.string.toast_cleaning))
                return true
            }
        }
        return super.onKeyDown(keyCode, event)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            android.R.id.home -> if (isCleaning) {
                toast(resStr(R.string.toast_cleaning))
                return true
            }
            1 -> threadClean()
        }
        return true
    }

    private fun threadClean() {
        tvClean.append(getString(R.string.view_start_clean))
        miRun.isEnabled = false
        isCleaning = true
        thread { DeviceAPI.systemClean(this@CleanActivity) }
    }

    private inner class CleanCallbackReceiver : BroadcastReceiver() {
        override fun onReceive(context: Context?, intent: Intent?) {
            runOnMainThread {
                val inCallback = intent!!
                val status = inCallback.getIntExtra(KEY_STATUS, -1)
                val data = inCallback.getStringExtra(KEY_DATA)
                if (status == DeviceAPI.STATUS_PROGRESS || status == DeviceAPI.STATUS_ERROR) {
                    tvClean.append(data + "\n")
                } else if (status == DeviceAPI.STATUS_COMPLETE) {
                    tvClean.append(data + "\n")
                    isCleaning = false
                    miRun.isEnabled = true
                }
                svClean.fullScroll(ScrollView.FOCUS_DOWN)
            }
        }
    }
}
