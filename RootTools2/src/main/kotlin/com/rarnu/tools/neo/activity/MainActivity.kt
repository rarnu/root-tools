package com.rarnu.tools.neo.activity

import android.Manifest
import android.app.Fragment
import android.content.SharedPreferences
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import android.util.Log
import android.widget.Toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.base.BaseActivity
import com.rarnu.tools.neo.fragment.MainFragment
import com.rarnu.tools.neo.utils.AppUtils
import com.rarnu.tools.neo.utils.UIUtils
import com.rarnu.tools.neo.xposed.XpStatus

class MainActivity : BaseActivity() {

    private var pref: SharedPreferences? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        UIUtils.initDisplayMetrics(this, windowManager, true)
        super.onCreate(savedInstanceState)
        pref = getSharedPreferences(XpStatus.PREF, if (Build.VERSION.SDK_INT < 24) 1 else 0)
        XpStatus.mode = if (pref!!.getBoolean(XpStatus.KEY_WORK_MODE, false)) XpStatus.Mode.NDK else XpStatus.Mode.JVM

        DeviceAPI.isRejected = !DeviceAPI.mount()
        DeviceAPI.isSystemRW = DeviceAPI.isSystemRW
        val isRooted = !DeviceAPI.isRejected
        Log.e("DeviceAPI", "isRejected => " + DeviceAPI.isRejected)
        Log.e("DeviceAPI", "isSystemRW => " + DeviceAPI.isSystemRW)

        if (!DeviceAPI.isSystemRW) {
            Toast.makeText(this, R.string.toast_need_crack_system, Toast.LENGTH_SHORT).show()
        }

        val xpEnabled = XpStatus.isEnable()

        if (!xpEnabled && !isRooted) {
            Toast.makeText(this, R.string.toast_need_root_xposed, Toast.LENGTH_SHORT).show()
        } else if (!xpEnabled && isRooted) {
            Toast.makeText(this, R.string.toast_need_xposed, Toast.LENGTH_SHORT).show()
        } else if (xpEnabled && !isRooted) {
            Toast.makeText(this, R.string.toast_need_root, Toast.LENGTH_SHORT).show()
        }

        AppUtils.doScanMedia(this)
        requirePermission()
    }

    override fun getIcon(): Int = R.drawable.ic_launcher

    override fun replaceFragment(): Fragment = MainFragment()

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = false

    @SuppressWarnings("Duplicates")
    private fun requirePermission() {
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

    // No override here for compact with 5.0
    // @Override
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
}
