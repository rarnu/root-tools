package com.rarnu.tools.neo.activity

import android.app.Fragment
import com.rarnu.base.app.BaseActivity
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.fragment.SettingsFragment

/**
 * Created by rarnu on 11/23/16.
 */
class SettingsActivity : BaseActivity() {
    override fun getIcon(): Int = R.drawable.ic_launcher

    override fun replaceFragment(): Fragment = SettingsFragment()

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true
}
