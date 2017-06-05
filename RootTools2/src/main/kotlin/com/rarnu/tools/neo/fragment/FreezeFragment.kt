package com.rarnu.tools.neo.fragment

import android.app.AlertDialog
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import android.widget.Toast
import com.rarnu.base.app.BaseFragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.AppAdapter
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.data.AppInfo
import com.rarnu.tools.neo.loader.AppLoader
import kotlinx.android.synthetic.main.fragment_freeze.view.*
import kotlin.concurrent.thread

class FreezeFragment : BaseFragment(), AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener, SearchView.OnQueryTextListener {

    private var list: MutableList<AppInfo>? = null
    private var adapter: AppAdapter? = null
    private var loader: AppLoader? = null
    private var sv: SearchView? = null
    private var miSearch: MenuItem? = null

    override fun onItemClick(parent: AdapterView<*>, view: View, position: Int, id: Long) {
        val item = adapter?.getItem(position) as AppInfo?
        threadChangeAppFreeze(item)
    }

    override fun onQueryTextSubmit(query: String): Boolean = false

    override fun onQueryTextChange(newText: String): Boolean {
        adapter?.filter(newText)
        return true
    }

    override fun getBarTitle(): Int = R.string.freeze_name

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        list = arrayListOf<AppInfo>()
        adapter = AppAdapter(context, list)
        adapter?.setShowSwitch(true)
        innerView.lvApp.adapter = adapter
        loader = AppLoader(context)
    }

    override fun initEvents() {
        innerView.lvApp.onItemClickListener = this
        innerView.lvApp.onItemLongClickListener = this

        loader?.registerListener(0, { _, data ->
            list?.clear()
            if (data != null) {
                list?.addAll(data)
            }
            adapter?.setNewList(list)
            innerView.loading.visibility = View.GONE
        })

    }

    override fun initLogic() {
        innerView.loading.visibility = View.VISIBLE
        loader?.startLoading()
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_freeze

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu) {
        sv = SearchView(context)
        sv?.setOnQueryTextListener(this)
        menu.clear()
        miSearch = menu.add(0, 1, 1, R.string.ab_search)
        miSearch?.setIcon(android.R.drawable.ic_menu_search)
        miSearch?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        miSearch?.actionView = sv
    }

    override fun onGetNewArguments(bn: Bundle?) {

    }

    override fun getFragmentState(): Bundle? = null

    private val hFreeze = object : Handler() {
        override fun handleMessage(msg: Message) {
            if (msg.what == 0) {
                Toast.makeText(context, R.string.toast_freeze_fail, Toast.LENGTH_SHORT).show()
            }
            adapter?.notifyDataSetChanged()
            innerView.lvApp.isEnabled = true
            super.handleMessage(msg)
        }
    }

    private fun threadChangeAppFreeze(item: AppInfo?) {
        innerView.lvApp.isEnabled = false
        thread {
            val newStat = !item!!.isDisable
            val ret = DeviceAPI.freezeApplication(item.packageName, newStat)
            if (ret) {
                item.isDisable = newStat
            }
            val msg = Message()
            msg.what = if (ret) 1 else 0
            msg.obj = item
            hFreeze.sendMessage(msg)
        }
    }

    private fun showDeleteAppDialog(item: AppInfo, isSystemRequired: Boolean) {
        // delete app
        if (isSystemRequired) {
            AlertDialog.Builder(context).setTitle(R.string.alert_hint)
                    .setMessage(R.string.alert_cannot_delete_app).setPositiveButton(R.string.alert_ok, null).show()
        } else {
            AlertDialog.Builder(context).setTitle(R.string.alert_hint)
                    .setMessage(getString(R.string.alert_delete_app, item.name))
                    .setPositiveButton(R.string.alert_ok) { dialog, which -> doDeleteApp(item) }.setNegativeButton(R.string.alert_cancel, null)
                    .show()
        }

    }

    private val hDeleteApp = object : Handler() {
        override fun handleMessage(msg: Message) {
            when (msg.what) {
                0 ->
                    // delete fail
                    Toast.makeText(context, R.string.toast_delete_system_app_fail, Toast.LENGTH_SHORT).show()
                1 -> {
                    // delete succ
                    Toast.makeText(context, R.string.toast_delete_system_app_succ, Toast.LENGTH_SHORT).show()
                    innerView.loading.visibility = View.VISIBLE
                    loader?.startLoading()
                }
            }
            super.handleMessage(msg)
        }
    }

    private fun doDeleteApp(item: AppInfo) {
        // delete app
        thread {
            val ret = DeviceAPI.deleteSystemApp(item.packageName)
            hDeleteApp.sendEmptyMessage(if (ret) 1 else 0)
        }
    }

    override fun onItemLongClick(parent: AdapterView<*>, view: View, position: Int, id: Long): Boolean {
        val item = adapter?.getItem(position) as AppInfo?
        if (item!!.isSystem) {
            showDeleteAppDialog(item, DeviceAPI.isAppRequiredBySystem(item.packageName))
        }
        return true
    }
}
