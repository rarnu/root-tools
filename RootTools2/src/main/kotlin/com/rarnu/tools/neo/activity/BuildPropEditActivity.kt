package com.rarnu.tools.neo.activity

import android.app.Fragment
import com.rarnu.base.app.BaseDialog
import com.rarnu.tools.neo.fragment.BuildPropEditFragment

class BuildPropEditActivity : BaseDialog() {

    override fun getCloseCondition(): Boolean = false

    override fun replaceFragment(): Fragment = BuildPropEditFragment()

    override fun customTheme(): Int = 0
}
