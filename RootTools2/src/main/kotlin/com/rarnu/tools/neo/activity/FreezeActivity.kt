@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import com.rarnu.android.*
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.AppAdapter
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.data.AppInfo
import com.rarnu.tools.neo.loader.AppLoader
import kotlinx.android.synthetic.main.fragment_freeze.*
import kotlin.concurrent.thread

class FreezeActivity : BackActivity(), AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener, SearchView.OnQueryTextListener {

    private var list = mutableListOf<AppInfo>()
    private lateinit var adapter: AppAdapter
    private lateinit var loader: AppLoader
    private lateinit var sv: SearchView
    private lateinit var miSearch: MenuItem

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_freeze)
        actionBar?.title = resStr(R.string.freeze_name)

        adapter = AppAdapter(this, list)
        adapter.setShowSwitch(true)
        lvApp.adapter = adapter
        loader = AppLoader(this)

        lvApp.onItemClickListener = this
        lvApp.onItemLongClickListener = this

        loader.registerListener(0) { _, data ->
            list.clear()
            if (data != null) {
                list.addAll(data)
            }
            adapter.setNewList(list)
            loading.visibility = View.GONE
        }

        loading.visibility = View.VISIBLE
        loader.startLoading()

    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        sv = SearchView(this)
        sv.setOnQueryTextListener(this)
        miSearch = menu.add(0, 1, 1, R.string.ab_search)
        miSearch.setIcon(android.R.drawable.ic_menu_search)
        miSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        miSearch.actionView = sv
        return super.onCreateOptionsMenu(menu)
    }

    override fun onItemClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        val item = adapter.getItem(position) as AppInfo
        threadChangeAppFreeze(item)
    }

    private fun threadChangeAppFreeze(item: AppInfo) {
        lvApp.isEnabled = false
        thread {
            val newStat = !item.isDisable
            val ret = DeviceAPI.freezeApplication(item.packageName, newStat)
            if (ret) {
                item.isDisable = newStat
            }
            runOnMainThread {
                if (!ret) {
                    toast(resStr(R.string.toast_freeze_fail))
                }
                adapter.notifyDataSetChanged()
                lvApp.isEnabled = true
            }
        }
    }

    override fun onItemLongClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long): Boolean {
        val item = adapter.getItem(position) as AppInfo
        if (item.isSystem) {
            showDeleteAppDialog(item, DeviceAPI.isAppRequiredBySystem(item.packageName))
        }
        return true
    }

    private fun showDeleteAppDialog(item: AppInfo, isSystemRequired: Boolean) {
        // delete app
        if (isSystemRequired) {
            alert(resStr(R.string.alert_hint),
                    resStr(R.string.alert_cannot_delete_app),
                    resStr(R.string.alert_ok)) {}
        } else {
            alert(resStr(R.string.alert_hint),
                    resStr(R.string.alert_delete_app, item.name),
                    resStr(R.string.alert_ok),
                    resStr(R.string.alert_cancel)) {
                if (it == 0) {
                    doDeleteApp(item)
                }
            }
        }
    }

    private fun doDeleteApp(item: AppInfo) {
        // delete app
        thread {
            val ret = DeviceAPI.deleteSystemApp(item.packageName)
            runOnMainThread {
                if (ret) {
                    // delete succ
                    toast(resStr(R.string.toast_delete_system_app_succ))
                    loading.visibility = View.VISIBLE
                    loader.startLoading()
                } else {
                    toast(resStr(R.string.toast_delete_system_app_fail))
                }
            }
        }
    }

    override fun onQueryTextSubmit(query: String?) = false

    override fun onQueryTextChange(newText: String?): Boolean {
        adapter.filter(newText)
        return true
    }

}
