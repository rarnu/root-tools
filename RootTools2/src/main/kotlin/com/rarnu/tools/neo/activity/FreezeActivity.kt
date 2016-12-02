package com.rarnu.tools.neo.activity

import android.app.Fragment
import android.os.Bundle
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.base.BaseActivity
import com.rarnu.tools.neo.fragment.FreezeFragment

class FreezeActivity : BaseActivity() {

    override fun getIcon(): Int = R.drawable.ic_launcher

    override fun replaceFragment(): Fragment = FreezeFragment()

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true
}
