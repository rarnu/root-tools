package com.rarnu.tools.neo.activity

import android.app.Fragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.base.BaseActivity
import com.rarnu.tools.neo.fragment.ComponentFragment

class ComponentActivity : BaseActivity() {

    override fun getIcon(): Int = R.drawable.ic_launcher

    override fun replaceFragment(): Fragment = ComponentFragment()

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true
}
