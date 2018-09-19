package com.rarnu.tools.neo.activity

import android.app.Activity
import android.graphics.Color
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import android.widget.TextView
import com.rarnu.kt.android.*
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.CompDetailAdapter
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.loader.ComponentLoader
import com.rarnu.tools.neo.utils.CompInfo
import com.rarnu.tools.neo.utils.ComponentUtils
import kotlinx.android.synthetic.main.fragment_component_detail.*
import kotlin.concurrent.thread

class ComponentDetailActivity : Activity(), SearchView.OnQueryTextListener, AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener, View.OnClickListener {



    private var pkgName = ""
    private var versionCode = 0
    private var focusItem = -1

    private var list = arrayListOf<CompInfo>()
    private lateinit var adapter: CompDetailAdapter
    private lateinit var loader: ComponentLoader

    private lateinit var miSearch: MenuItem
    private lateinit var sv: SearchView
    private var filterText = ""

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_component_detail)
        actionBar?.title = intent.getStringExtra("name")
        showActionBack()

        adapter = CompDetailAdapter(this, list)
        lvComponent.adapter = adapter
        loader = ComponentLoader(this)

        lvComponent.onItemClickListener = this
        lvComponent.onItemLongClickListener = this
        btnActivity.setOnClickListener(this)
        btnService.setOnClickListener(this)
        btnReceiver.setOnClickListener(this)
        btnProvider.setOnClickListener(this)
        btnProfile.setOnClickListener(this)
        btnDownloadProfile.setOnClickListener { threadGetOnekeyAndApply() }
        btnUploadProfile.setOnClickListener { threadPutOnekeyConfig() }

        loader.registerListener(0) { _, data ->
            if (data != null) {
                list.addAll(data)
                adapter.setNewList(list)
                if (filterText != "") {
                    adapter.filter(filterText)
                }
            }
            loading.visibility = View.GONE
        }

        pkgName = intent.getStringExtra("pkg")
        versionCode = intent.getIntExtra("versionCode", 0)
        tvPkgName.text = getString(R.string.view_profile_pkg, pkgName)
        tvVer.text = getString(R.string.view_profile_ver, versionCode)
        onClick(btnActivity)
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        menu.clear()
        sv = SearchView(this)
        sv.setOnQueryTextListener(this)
        miSearch = menu.add(0, 1, 1, R.string.ab_search)
        miSearch.setIcon(android.R.drawable.ic_menu_search)
        miSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        miSearch.actionView = sv
        return super.onCreateOptionsMenu(menu)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when(item.itemId) {
            android.R.id.home -> finish()
        }
        return true
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
        val item = adapter.getItem(position) as CompInfo
        threadChangeComponentFreeze(item)
    }

    override fun onItemLongClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long): Boolean {
        // long click for intent filter detail
        val item = adapter.getItem(position) as CompInfo
        val msg = item.intents
        var msgStr = ""
        if (msg.isEmpty()) {
            msgStr = getString(R.string.alert_no_intent)
        } else {
            for (s in msg) {
                msgStr += s + "\n"
            }
        }
        msgStr = "${item.fullPackageName}\n\n$msgStr"
        alert(resStr(R.string.alert_hint), msgStr, resStr(R.string.alert_ok)) { }
        return true
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

    private fun unfocusButtons() {
        btnActivity.background = resDrawable(R.drawable.button_normal)
        btnService.background = resDrawable(R.drawable.button_normal)
        btnReceiver.background = resDrawable(R.drawable.button_normal)
        btnProvider.background = resDrawable(R.drawable.button_normal)
        btnProfile.background = resDrawable(R.drawable.button_normal)
        btnActivity.setTextColor(Color.BLACK)
        btnService.setTextColor(Color.BLACK)
        btnReceiver.setTextColor(Color.BLACK)
        btnProvider.setTextColor(Color.BLACK)
        btnProfile.setTextColor(Color.BLACK)
    }

    private fun focusButton(btn: TextView) {
        btn.background = resDrawable(R.drawable.button_focus)
        btn.setTextColor(Color.WHITE)
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
            val ret = DeviceAPI.freezeComponent(pkgName, item.component?.className, !newStat)
            if (ret) {
                item.enabled = newStat
            }
            runOnUiThread {
                if (!ret) {
                    toast(resStr(R.string.toast_component_fail))
                }
                adapter.notifyDataSetChanged()
                lvComponent.isEnabled = true
            }
        }
    }

    private fun threadGetOnekeyAndApply() {
        btnDownloadProfile.isEnabled = false
        btnUploadProfile.isEnabled = false
        loading.visibility = View.VISIBLE
        thread {
            var hasProfile = false
            val ok = API.getOnekey(pkgName, versionCode)
            if (ok != null) {
                if (ok.disabledComponents != null && ok.disabledComponents!!.isNotEmpty()) {
                    hasProfile = true
                }
                if (hasProfile) {
                    DeviceAPI.freezeComponents(pkgName, ok.disabledComponents, true)
                }
            }
            runOnUiThread {
                showApplyProfileAlert(hasProfile)
                btnDownloadProfile.isEnabled = true
                btnUploadProfile.isEnabled = true
                loading.visibility = View.GONE
            }
        }
    }

    private fun threadPutOnekeyConfig() {
        // thread put onekey config
        btnDownloadProfile.isEnabled = false
        btnUploadProfile.isEnabled = false
        loading.visibility = View.VISIBLE
        thread {
            var ret = false
            try {
                val info = packageManager.getApplicationInfo(pkgName, 0)
                val ppu = PackageParserP.newPackageParser()
                val obj = ppu?.parsePackage(info.publicSourceDir, 0)
                val lstActivity = ComponentUtils.getActivityList(this@ComponentDetailActivity, obj)
                val lstService = ComponentUtils.getServiceList(this@ComponentDetailActivity, obj)
                val lstReceiver = ComponentUtils.getReceiverList(this@ComponentDetailActivity, obj)
                val lstProvider = ComponentUtils.getProviderList(this@ComponentDetailActivity, obj)
                val lstDisabled = arrayListOf<String>()
                lstActivity.filterNot { it.enabled }.mapTo(lstDisabled) { it.component?.className ?: "" }
                lstService.filterNot { it.enabled }.mapTo(lstDisabled) { it.component?.className ?: "" }
                lstReceiver.filterNot { it.enabled }.mapTo(lstDisabled) { it.component?.className ?: "" }
                lstProvider.filterNot { it.enabled }.mapTo(lstDisabled) { it.component?.className ?: "" }
                if (lstDisabled.size != 0) {
                    ret = API.uploadOnekey(pkgName, versionCode, lstDisabled)
                }
            } catch (e: Exception) {

            }
            runOnUiThread {
                showUploadProfileAlert(ret)
                btnDownloadProfile.isEnabled = true
                btnUploadProfile.isEnabled = true
                loading.visibility = View.GONE
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
