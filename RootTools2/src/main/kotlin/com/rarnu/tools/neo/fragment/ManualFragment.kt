package com.rarnu.tools.neo.fragment

import android.os.Bundle
import android.view.Menu
import android.webkit.WebSettings
import com.rarnu.base.app.BaseFragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import kotlinx.android.synthetic.main.fragment_manual.view.*

/**
 * Created by rarnu on 12/13/16.
 */
class ManualFragment : BaseFragment() {

    override fun getBarTitle(): Int = R.string.view_usage_manual

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        innerView.wvManual.settings.javaScriptEnabled = false
        innerView.wvManual.settings.layoutAlgorithm = WebSettings.LayoutAlgorithm.SINGLE_COLUMN
        innerView.wvManual.settings.displayZoomControls = false
        innerView.wvManual.settings.builtInZoomControls = false
    }

    override fun initEvents() {}

    override fun initLogic() {
        innerView.wvManual.loadUrl(API.API_BASE + if (RootApplication.isZh) "manual.html" else "manual_en.html")
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_manual

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu) { }

    override fun onGetNewArguments(bn: Bundle?) {}

    override fun getFragmentState(): Bundle? = null
}