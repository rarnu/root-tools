package com.rarnu.tools.neo.base

import android.os.Bundle
import android.view.Menu

/**
 * Created by rarnu on 3/23/16.
 */
interface IIntf {
    fun getBarTitle(): Int
    fun getCustomTitle(): String?
    fun initComponents()
    fun initEvents()
    fun initLogic()
    fun getFragmentLayoutResId(): Int
    fun getMainActivityName(): String?
    fun initMenu(menu: Menu?)
    fun onGetNewArguments(bn: Bundle?)
    fun getFragmentState(): Bundle?
}