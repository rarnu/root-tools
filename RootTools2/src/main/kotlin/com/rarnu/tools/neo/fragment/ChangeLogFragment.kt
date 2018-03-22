package com.rarnu.tools.neo.fragment

import android.os.Bundle
import android.view.Menu
import com.rarnu.base.app.BaseFragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import kotlinx.android.synthetic.main.fragment_changelog.view.*
import kotlin.concurrent.thread

/**
 * Created by rarnu on 12/7/16.
 */
class ChangeLogFragment : BaseFragment() {

    override fun getBarTitle(): Int = R.string.view_changelog

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() { }

    override fun initEvents() { }

    override fun initLogic() {
        thread {
            val list = API.getAllUpdateInfo()
            activity.runOnUiThread {
                var str = ""
                list?.filterNotNull()?.filter {
                    if (RootApplication.isZh) {
                        it.description.trim { it <= ' ' } != ""
                    } else {
                        it.descriptionEn.trim { it <= ' ' } != ""
                    }
                }?.forEach { str += "${it.versionName} (${it.versionCode})\n\n    ${if (RootApplication.isZh) it.description else it.descriptionEn}\n\n" }
                str = str.replace("\\n", "\n    ")
                innerView.tvChangeLog.text = str
            }
        }
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_changelog

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}