package com.rarnu.tools.neo.activity

import android.app.Fragment
import android.os.Bundle
import android.view.KeyEvent
import android.view.MenuItem
import android.widget.Toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.base.BaseActivity
import com.rarnu.tools.neo.fragment.CleanFragment

class CleanActivity : BaseActivity() {

    private val cf = CleanFragment()

    override fun getIcon(): Int = R.drawable.ic_launcher

    override fun replaceFragment(): Fragment = cf

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true

    private val isCleaning: Boolean
        get() {
            val bn = cf.getFragmentState()
            if (bn != null) {
                return bn.getBoolean("isCleaning", false)
            } else {
                return false
            }
        }

    override fun onKeyDown(keyCode: Int, event: KeyEvent): Boolean {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (isCleaning) {
                Toast.makeText(this, R.string.toast_cleaning, Toast.LENGTH_SHORT).show()
                return true
            }
        }
        return super.onKeyDown(keyCode, event)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            android.R.id.home -> if (isCleaning) {
                Toast.makeText(this, R.string.toast_cleaning, Toast.LENGTH_SHORT).show()
                return true
            }
        }
        return super.onOptionsItemSelected(item)
    }
}
