package com.rarnu.tools.neo.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.widget.TextView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.base.BaseFragment
import com.rarnu.tools.neo.data.UpdateInfo
import java.util.*
import kotlin.concurrent.thread

/**
 * Created by rarnu on 12/7/16.
 */
class ChangeLogFragment : BaseFragment() {

    private var tvChangeLog: TextView? = null

    override fun getBarTitle(): Int = R.string.view_changelog

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        tvChangeLog = innerView?.findViewById(R.id.tvChangeLog) as TextView?
    }

    override fun initEvents() {

    }

    private val hChangeLog = object : Handler() {
        override fun handleMessage(msg: Message) {
            val info = msg.obj as MutableList<UpdateInfo?>?
            var str = ""
            info?.filterNotNull()?.forEach { str += "${it.versionName} (${it.versionCode})\n\n    ${if (RootApplication.isZh) it.description else it.descriptionEn}\n\n" }
            str = str.replace("\\n", "\n    ")
            tvChangeLog?.text = str
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

    override fun initMenu(menu: Menu?) {
    }

    override fun onGetNewArguments(bn: Bundle?) {
    }

    override fun getFragmentState(): Bundle? = null

}