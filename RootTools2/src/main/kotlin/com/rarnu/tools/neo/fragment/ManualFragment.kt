package com.rarnu.tools.neo.fragment

import android.os.Bundle
import android.view.Menu
import android.webkit.WebSettings
import android.webkit.WebView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.base.BaseFragment
import java.util.*

/**
 * Created by rarnu on 12/13/16.
 */
class ManualFragment : BaseFragment() {

    private var wvManual: WebView? = null

    override fun getBarTitle(): Int = R.string.view_usage_manual

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        wvManual = innerView?.findViewById(R.id.wvManual) as WebView?
        wvManual?.settings?.javaScriptEnabled = false
        wvManual?.settings?.layoutAlgorithm = WebSettings.LayoutAlgorithm.SINGLE_COLUMN
        wvManual?.settings?.displayZoomControls = false
        wvManual?.settings?.builtInZoomControls = false
    }

    override fun initEvents() {

    }

    override fun initLogic() {
        wvManual?.loadUrl(API.API_BASE + if (RootApplication.isZh) "manual.html" else "manual_en.html")
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_manual

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) {

    }

    override fun onGetNewArguments(bn: Bundle?) {

    }

    override fun getFragmentState(): Bundle? = null
}