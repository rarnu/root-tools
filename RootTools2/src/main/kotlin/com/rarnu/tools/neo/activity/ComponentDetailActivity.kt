@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.graphics.Color
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import android.widget.TextView
import com.rarnu.android.*
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.CompDetailAdapter
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.loader.ComponentLoader
import com.rarnu.tools.neo.utils.CompInfo
import kotlinx.android.synthetic.main.fragment_component_detail.*
import kotlin.concurrent.thread

class ComponentDetailActivity : BackActivity(), SearchView.OnQueryTextListener, AdapterView.OnItemClickListener, View.OnClickListener {

    private var pkgName = ""
    private var versionCode = 0
    private var focusItem = -1

    private val list = mutableListOf<CompInfo>()
    private val adapter: CompDetailAdapter
    private val loader: ComponentLoader

    private var filterText = ""

    init {
        adapter = CompDetailAdapter(this, list)
        loader = ComponentLoader(this).apply {
            registerListener(0) { _, data ->
                if (data != null) {
                    list.addAll(data)
                    adapter.setNewList(list)
                    if (filterText != "") {
                        adapter.filter(filterText)
                    }
                }
                loading.visibility = View.GONE
            }
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_component_detail)
        actionBar?.title = intent.getStringExtra("name")
        lvComponent.adapter = adapter
        lvComponent.onItemClickListener = this
        btnActivity.setOnClickListener(this)
        btnService.setOnClickListener(this)
        btnReceiver.setOnClickListener(this)
        btnProvider.setOnClickListener(this)
        btnProfile.setOnClickListener(this)
        btnDownloadProfile.setOnClickListener { }
        btnUploadProfile.setOnClickListener { }
        pkgName = intent.getStringExtra("pkg")
        versionCode = intent.getIntExtra("versionCode", 0)
        tvPkgName.text = getString(R.string.view_profile_pkg, pkgName)
        tvVer.text = getString(R.string.view_profile_ver, versionCode)
        onClick(btnActivity)
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        menu.add(0, 1, 1, R.string.ab_search).apply {
            setIcon(android.R.drawable.ic_menu_search)
            setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
            actionView = SearchView(this@ComponentDetailActivity).apply {
                setOnQueryTextListener(this@ComponentDetailActivity)
            }
        }
        return super.onCreateOptionsMenu(menu)
    }

    override fun onQueryTextSubmit(query: String?) = false

    override fun onQueryTextChange(newText: String?): Boolean {
        if (newText != null) {
            filterText = newText
            adapter.filter(newText)
        }
        return true
    }

    override fun onItemClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        threadChangeComponentFreeze(adapter.getItem(position) as CompInfo)
    }

    override fun onClick(v: View) {
        lvComponent.visibility = View.VISIBLE
        layProfile.visibility = View.GONE
        unfocusButtons()
        when (v.id) {
            R.id.btnActivity -> if (focusItem != 0) {
                doLoadData(0)
                focusButton(btnActivity)
            }
            R.id.btnService -> if (focusItem != 1) {
                doLoadData(1)
                focusButton(btnService)
            }
            R.id.btnReceiver -> if (focusItem != 2) {
                doLoadData(2)
                focusButton(btnReceiver)
            }
            R.id.btnProvider -> if (focusItem != 3) {
                doLoadData(3)
                focusButton(btnProvider)
            }
            R.id.btnProfile -> {
                focusItem = 4
                lvComponent.visibility = View.GONE
                layProfile.visibility = View.VISIBLE
                focusButton(btnProfile)
            }
        }
    }

    private fun unfocusButtons() = arrayOf(btnActivity, btnService, btnReceiver, btnProvider, btnProfile).forEach {
        unfocusButton(it)
    }

    private fun focusButton(btn: TextView) = with(btn) {
        background = resDrawable(R.drawable.button_focus)
        setTextColor(Color.WHITE)
    }

    private fun unfocusButton(btn: TextView) = with(btn) {
        background = resDrawable(R.drawable.button_normal)
        setTextColor(Color.WHITE)
    }

    private fun doLoadData(type: Int) {
        loading.visibility = View.VISIBLE
        focusItem = type
        list.clear()
        adapter.notifyDataSetChanged()
        loader.startLoading(pkgName, type)
    }

    private fun threadChangeComponentFreeze(item: CompInfo) {
        lvComponent.isEnabled = false
        thread {
            val newStat = !item.enabled
            val ret = DeviceAPI.freezeComponent(pkgName, item.componentClassName, !newStat)
            if (ret) {
                item.enabled = newStat
            }
            runOnMainThread {
                if (!ret) {
                    toast(resStr(R.string.toast_component_fail))
                }
                adapter.notifyDataSetChanged()
                lvComponent.isEnabled = true
            }
        }
    }

    private fun showApplyProfileAlert(hasProfile: Boolean) = alert(resStr(R.string.alert_hint),
            resStr(if (hasProfile) R.string.alert_apply_component_config else R.string.alert_no_component_config),
            resStr(R.string.alert_ok)) { }

    private fun showUploadProfileAlert(succ: Boolean) = alert(resStr(R.string.alert_hint),
            resStr(if (succ) R.string.alert_upload_ok else R.string.alert_upload_failed),
            resStr(R.string.alert_ok)) { }

}
