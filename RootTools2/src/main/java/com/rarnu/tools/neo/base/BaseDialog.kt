package com.rarnu.tools.neo.base

import android.app.Activity
import android.app.Fragment
import android.os.Bundle

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseDialog: Activity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        if (customTheme() != 0) {
            setTheme(customTheme())
        }
        super.onCreate(savedInstanceState)
        if (getCloseCondition()) {
            finish()
            return
        }
        replace()
    }

    open fun replace() {
        val bf = replaceFragment()
        fragmentManager.beginTransaction().replace(android.R.id.content, bf).commit()
    }

    abstract fun getCloseCondition(): Boolean
    abstract fun replaceFragment(): Fragment
    abstract fun customTheme(): Int
}