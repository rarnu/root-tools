package com.rarnu.tools.neo.activity

import android.Manifest
import android.app.Fragment
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.base.BaseActivity
import com.rarnu.tools.neo.fragment.FeedbackFragment
import com.rarnu.tools.neo.xposed.XpStatus

/**
 * Created by rarnu on 11/19/16.
 */
class FeedbackActivity : BaseActivity() {

    @SuppressWarnings("Duplicates")
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
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

    override fun getIcon(): Int = R.drawable.ic_launcher

    override fun replaceFragment(): Fragment = FeedbackFragment()

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true

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
