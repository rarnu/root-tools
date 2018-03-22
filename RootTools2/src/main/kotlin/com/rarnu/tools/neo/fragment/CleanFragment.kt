package com.rarnu.tools.neo.fragment

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.*
import android.view.Menu
import android.view.MenuItem
import android.widget.ScrollView
import com.rarnu.base.app.BaseFragment
import com.rarnu.base.utils.FileUtils
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.DeviceAPI
import kotlinx.android.synthetic.main.fragment_clean.view.*
import java.io.File
import kotlin.concurrent.thread

class CleanFragment : BaseFragment() {

    private var miRun: MenuItem? = null
    private var isCleaning = false

    private var filterCallback: IntentFilter? = null
    private var receiverCallback: CleanCallbackReceiver? = null

    override fun getBarTitle(): Int = R.string.clean_name

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() {}

    override fun initEvents() {}

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
            innerView.tvClean.setText(R.string.view_ready)
            return
        }
        if (busyboxExists) {
            innerView.tvClean.setText(R.string.view_ready)
            return
        }
        innerView.tvClean.setText(R.string.view_not_ready)
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
        FileUtils.copyAssetFile(context, busyboxAsset, fDir.absolutePath, null)
        DeviceAPI.mount()
        val ret = DeviceAPI.catFile(fBusybox.absolutePath, "/system/xbin/busybox", 755)
        activity.runOnUiThread {
            if (ret) {
                innerView.tvClean.setText(R.string.view_ready)
            } else {
                innerView.tvClean.setText(R.string.view_env_error)
                miRun?.isEnabled = false
            }
        }
    }


    override fun getFragmentLayoutResId(): Int = R.layout.fragment_clean

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu) {
        menu.clear()
        miRun = menu.add(0, 1, 1, R.string.ab_clean)
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
        innerView.tvClean.append(getString(R.string.view_start_clean))
        miRun?.isEnabled = false
        isCleaning = true
        thread { DeviceAPI.systemClean(context) }
    }

    private inner class CleanCallbackReceiver : BroadcastReceiver() {
        override fun onReceive(context: Context?, intent: Intent?) {
            activity.runOnUiThread {
                val inCallback = intent!!
                val status = inCallback.getIntExtra(KEY_STATUS, -1)
                val data = inCallback.getStringExtra(KEY_DATA)
                if (status == DeviceAPI.STATUS_PROGRESS || status == DeviceAPI.STATUS_ERROR) {
                    innerView.tvClean.append(data + "\n")
                } else if (status == DeviceAPI.STATUS_COMPLETE) {
                    innerView.tvClean.append(data + "\n")
                    isCleaning = false
                    miRun?.isEnabled = true
                }
                innerView.svClean.fullScroll(ScrollView.FOCUS_DOWN)
            }
        }
    }

    companion object {
        const val ACTION_CLEAN_CALLBACK = "com.rarnu.tools.neo.clean.callback"
        const val KEY_STATUS = "status"
        const val KEY_DATA = "data"
    }
}
