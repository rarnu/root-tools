package com.rarnu.tools.neo.fragment

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.*
import android.view.Menu
import android.view.MenuItem
import android.widget.ScrollView
import android.widget.TextView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.base.BaseFragment
import com.rarnu.tools.neo.utils.FileUtils

import java.io.File
import kotlin.concurrent.thread

class CleanFragment : BaseFragment() {

    private var tvClean: TextView? = null
    private var miRun: MenuItem? = null
    private var svClean: ScrollView? = null
    private var isCleaning = false

    private var filterCallback: IntentFilter? = null
    private var receiverCallback: CleanCallbackReceiver? = null

    override fun getBarTitle(): Int = R.string.clean_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        svClean = innerView?.findViewById(R.id.svClean) as ScrollView?
        tvClean = innerView?.findViewById(R.id.tvClean) as TextView?
    }

    override fun initEvents() {

    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        filterCallback = IntentFilter(ACTION_CLEAN_CALLBACK)
        receiverCallback = CleanCallbackReceiver()
        activity.registerReceiver(receiverCallback, filterCallback)
    }

    override fun onDestroy() {
        activity.unregisterReceiver(receiverCallback)
        super.onDestroy()
    }

    override fun initLogic() {
        val busyboxExists = File("/system/bin/busybox").exists() || File("/system/xbin/busybox").exists()
        val duExists = File("/system/bin/du").exists() || File("/system/xbin/du").exists()
        if (duExists) {
            tvClean?.setText(R.string.view_ready)
            return
        }
        if (busyboxExists) {
            tvClean?.setText(R.string.view_ready)
            return
        }
        tvClean?.setText(R.string.view_not_ready)
        threadExtractBusybox()
    }

    private val hEnvReady = object : Handler() {
        override fun handleMessage(msg: Message) {
            if (msg.what == 0) {
                tvClean?.setText(R.string.view_env_error)
                miRun?.isEnabled = false
            } else {
                tvClean?.setText(R.string.view_ready)
            }
            super.handleMessage(msg)
        }
    }

    private fun threadExtractBusybox() {

        thread {
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
            FileUtils.copyAssetFile(context, busyboxAsset, fDir.absolutePath)
            DeviceAPI.mount()
            val ret = DeviceAPI.catFile(fBusybox.absolutePath, "/system/xbin/busybox", 755)
            val msg = Message()
            msg.what = if (ret) 1 else 0
            hEnvReady.sendMessage(msg)
        }
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_clean

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) {
        menu?.clear()
        miRun = menu?.add(0, 1, 1, R.string.ab_clean)
        miRun?.setIcon(android.R.drawable.ic_menu_delete)
        miRun?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            1 -> threadClean()
        }
        return true
    }

    override fun onGetNewArguments(bn: Bundle?) {

    }

    override fun getFragmentState(): Bundle? {
        val bn = Bundle()
        bn.putBoolean("isCleaning", isCleaning)
        return bn
    }

    private fun threadClean() {
        tvClean?.append(getString(R.string.view_start_clean))
        miRun?.isEnabled = false
        isCleaning = true
        thread { DeviceAPI.systemClean(context) }
    }

    private val hCallback = object : Handler() {
        override fun handleMessage(msg: Message) {
            val inCallback = msg.obj as Intent
            val status = inCallback.getIntExtra(KEY_STATUS, -1)
            val data = inCallback.getStringExtra(KEY_DATA)
            if (status == DeviceAPI.STATUS_PROGRESS || status == DeviceAPI.STATUS_ERROR) {
                tvClean?.append(data + "\n")
            } else if (status == DeviceAPI.STATUS_COMPLETE) {
                tvClean?.append(data + "\n")
                isCleaning = false
                miRun?.isEnabled = true
            }
            svClean?.fullScroll(ScrollView.FOCUS_DOWN)
            super.handleMessage(msg)
        }
    }

    private inner class CleanCallbackReceiver : BroadcastReceiver() {
        override fun onReceive(context: Context, intent: Intent) {
            val msg = Message()
            msg.obj = intent
            hCallback.sendMessage(msg)
        }
    }

    companion object {
        val ACTION_CLEAN_CALLBACK = "com.rarnu.tools.neo.clean.callback"
        val KEY_STATUS = "status"
        val KEY_DATA = "data"
    }
}
