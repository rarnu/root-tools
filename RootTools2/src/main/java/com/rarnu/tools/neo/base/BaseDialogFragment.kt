package com.rarnu.tools.neo.base

import android.app.Fragment
import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseDialogFragment: Fragment(), IIntf {

    protected var innerView: View? = null

    override fun onCreateView(inflater: LayoutInflater?, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        innerView = inflater?.inflate(getFragmentLayoutResId(), container, false)
        initComponents()
        initEvents()
        initLogic()
        return innerView
    }

}