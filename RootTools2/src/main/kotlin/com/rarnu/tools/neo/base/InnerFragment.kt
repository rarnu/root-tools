package com.rarnu.tools.neo.base

import android.app.Fragment
import android.content.Context
import android.os.Build
import android.os.Bundle
import android.view.*

/**
 * Created by rarnu on 3/23/16.
 */
abstract class InnerFragment : Fragment, ViewTreeObserver.OnGlobalLayoutListener, IIntf {

    protected var innerView: View? = null
    protected var innerBundle: Bundle? = null
    var tabTitle: String? = null
    var tabIcon = -1

    constructor() : super()
    constructor(tabTitle: String) : super() {
        this.tabTitle = tabTitle
    }

    override fun onCreateView(inflater: LayoutInflater?, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        if (getFragmentLayoutResId() != 0) {
            innerView = inflater?.inflate(getFragmentLayoutResId(), container, false)
        } else {
            innerView = getFramgmentLayoutView()
        }
        initComponents()
        initEvents()
        innerView?.viewTreeObserver?.addOnGlobalLayoutListener(this)
        return innerView
    }

    override fun onActivityCreated(savedInstanceState: Bundle?) {
        super.onActivityCreated(savedInstanceState)
        innerBundle = innerBundle ?: arguments
        initLogic()
        if (activity.actionBar != null) {
            if (getCustomTitle() == null || getCustomTitle() == "") {
                if (getBarTitle() != 0) {
                    activity.actionBar.setTitle(getBarTitle())
                }
            } else {
                activity.actionBar.title = getCustomTitle()
            }
        }
    }

    open fun setNewArguments(bn: Bundle?) {
        innerBundle = bn
        onGetNewArguments(bn)
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setHasOptionsMenu(true)
    }

    override fun onCreateOptionsMenu(menu: Menu?, inflater: MenuInflater?) {
        if (activity == null) {
            return
        }
        initMenu(menu)
    }

    override fun getContext(): Context {
        if (Build.VERSION.SDK_INT >= 23) {
            return super.getContext()
        } else {
            return super.getActivity()
        }
    }

    /**
     * do NOT override this method, use @onLayoutReady instead
     */
    override fun onGlobalLayout() = onLayoutReady()

    /**
     * override the method if you want to re-layout after system layouted
     */
    protected open fun onLayoutReady() {
    }

    /**
     * override the method if you do not need a layout from resource and @getFragmentLayoutResId returns 0
     */
    protected open fun getFramgmentLayoutView(): View? = null
}