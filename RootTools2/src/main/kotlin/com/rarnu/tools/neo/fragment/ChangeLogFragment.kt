package com.rarnu.tools.neo.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.widget.TextView
import com.rarnu.base.app.BaseFragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.data.UpdateInfo
import kotlin.concurrent.thread
import kotlinx.android.synthetic.main.fragment_changelog.view.*

/**
 * Created by rarnu on 12/7/16.
 */
class ChangeLogFragment : BaseFragment() {

    override fun getBarTitle(): Int = R.string.view_changelog

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() { }

    override fun initEvents() { }

    private val hChangeLog = object : Handler() {
        override fun handleMessage(msg: Message) {
            val info = msg.obj as MutableList<UpdateInfo?>?
            var str = ""
            info?.filterNotNull()?.filter {
                if (RootApplication.isZh)
                    it.description.trim { it <= ' ' } != ""
                else
                    it.descriptionEn.trim { it <= ' ' } != ""
            }?.forEach { str += "${it.versionName} (${it.versionCode})\n\n    ${if (RootApplication.isZh) it.description else it.descriptionEn}\n\n" }
            str = str.replace("\\n", "\n    ")
            innerView.tvChangeLog.text = str
            super.handleMessage(msg)
        }
    }

    override fun initLogic() {
        thread {
            val list = API.getAllUpdateInfo()
            val msg = Message()
            msg.obj = list
            hChangeLog.sendMessage(msg)
        }
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_changelog

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}