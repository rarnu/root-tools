package com.rarnu.tools.neo.fragment

import android.app.AlertDialog
import android.graphics.Color
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import android.widget.TextView
import android.widget.Toast
import com.rarnu.base.app.BaseFragment
import com.rarnu.base.utils.ComponentUtils
import com.rarnu.base.utils.PackageParserUtils
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.CompDetailAdapter
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.loader.ComponentLoader
import kotlinx.android.synthetic.main.fragment_component_detail.view.*
import kotlin.concurrent.thread

class ComponentDetailFragment : BaseFragment(), View.OnClickListener, SearchView.OnQueryTextListener, AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener {

    private var pkgName = ""
    private var versionCode = 0
    private var focusItem = -1
    private var loader: ComponentLoader? = null
    private var list: MutableList<ComponentUtils.CompInfo>? = null
    private var adapter: CompDetailAdapter? = null

    private var miSearch: MenuItem? = null
    private var sv: SearchView? = null
    private var filterText = ""

    override fun onClick(v: View) {
        innerView.lvComponent.visibility = View.VISIBLE
        innerView.layProfile.visibility = View.GONE
        unfocusButtons()
        when (v.id) {
            R.id.btnActivity -> if (focusItem != 0) {
                doLoadData(0)
                focusButton(innerView.btnActivity)
            }
            R.id.btnService -> if (focusItem != 1) {
                doLoadData(1)
                focusButton(innerView.btnService)
            }
            R.id.btnReceiver -> if (focusItem != 2) {
                doLoadData(2)
                focusButton(innerView.btnReceiver)
            }
            R.id.btnProvider -> if (focusItem != 3) {
                doLoadData(3)
                focusButton(innerView.btnProvider)
            }
            R.id.btnProfile -> {
                focusItem = 4
                innerView.lvComponent.visibility = View.GONE
                innerView.layProfile.visibility = View.VISIBLE
                focusButton(innerView.btnProfile)
            }
        }
    }

    override fun onItemClick(parent: AdapterView<*>, view: View, position: Int, id: Long) {
        val item = adapter?.getItem(position) as ComponentUtils.CompInfo?
        threadChangeComponentFreeze(item)
    }

    override fun onItemLongClick(parent: AdapterView<*>, view: View, position: Int, id: Long): Boolean {
        // long click for intent filter detail
        val item = adapter?.getItem(position) as ComponentUtils.CompInfo?
        val msg = item?.intents
        var msgStr = ""
        if (msg == null || msg.isEmpty()) {
            msgStr = getString(R.string.alert_no_intent)
        } else {
            for (s in msg) {
                msgStr += s + "\n"
            }
        }
        msgStr = "${item?.fullPackageName}\n\n$msgStr"
        AlertDialog.Builder(context).setTitle(R.string.alert_hint).setMessage(msgStr).setPositiveButton(R.string.alert_ok, null).show()
        return true
    }

    override fun onQueryTextSubmit(query: String): Boolean = false

    override fun onQueryTextChange(newText: String): Boolean {
        filterText = newText
        adapter?.filter(newText)
        return true
    }

    override fun getBarTitle(): Int = 0

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = activity.intent.getStringExtra("name")

    override fun initComponents() {
        list = arrayListOf()
        adapter = CompDetailAdapter(context, list)
        innerView.lvComponent.adapter = adapter
        loader = ComponentLoader(context)
    }

    override fun initEvents() {
        innerView.lvComponent.onItemClickListener = this
        innerView.lvComponent.onItemLongClickListener = this
        innerView.btnActivity.setOnClickListener(this)
        innerView.btnService.setOnClickListener(this)
        innerView.btnReceiver.setOnClickListener(this)
        innerView.btnProvider.setOnClickListener(this)
        innerView.btnProfile.setOnClickListener(this)
        innerView.btnDownloadProfile.setOnClickListener { threadGetOnekeyAndApply() }
        innerView.btnUploadProfile.setOnClickListener { threadPutOnekeyConfig() }

        loader?.registerListener(0, { _, data ->
            if (data != null) {
                list?.addAll(data)
                adapter?.setNewList(list)
                if (filterText != "") {
                    adapter?.filter(filterText)
                }
            }
            innerView.loading.visibility = View.GONE
        })
    }

    override fun initLogic() {
        pkgName = activity.intent.getStringExtra("pkg")
        versionCode = activity.intent.getIntExtra("versionCode", 0)
        innerView.tvPkgName.text = getString(R.string.view_profile_pkg, pkgName)
        innerView.tvVer.text = getString(R.string.view_profile_ver, versionCode)
        onClick(innerView.btnActivity)
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_component_detail

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

    private fun unfocusButtons() {
        innerView.btnActivity.background = resources.getDrawable(R.drawable.button_normal)
        innerView.btnService.background = resources.getDrawable(R.drawable.button_normal)
        innerView.btnReceiver.background = resources.getDrawable(R.drawable.button_normal)
        innerView.btnProvider.background = resources.getDrawable(R.drawable.button_normal)
        innerView.btnProfile.background = resources.getDrawable(R.drawable.button_normal)
        innerView.btnActivity.setTextColor(Color.BLACK)
        innerView.btnService.setTextColor(Color.BLACK)
        innerView.btnReceiver.setTextColor(Color.BLACK)
        innerView.btnProvider.setTextColor(Color.BLACK)
        innerView.btnProfile.setTextColor(Color.BLACK)
    }

    private fun focusButton(btn: TextView) {
        btn.background = resources.getDrawable(R.drawable.button_focus)
        btn.setTextColor(Color.WHITE)
    }

    private fun doLoadData(type: Int) {
        innerView.loading.visibility = View.VISIBLE
        focusItem = type
        list?.clear()
        adapter?.notifyDataSetChanged()
        loader?.startLoading(pkgName, type)
    }

    private val hFreeze = object : Handler() {
        override fun handleMessage(msg: Message) {
            if (msg.what == 0) {
                Toast.makeText(context, R.string.toast_component_fail, Toast.LENGTH_SHORT).show()
            }
            adapter?.notifyDataSetChanged()
            innerView.lvComponent.isEnabled = true
            super.handleMessage(msg)
        }
    }

    private fun threadChangeComponentFreeze(item: ComponentUtils.CompInfo?) {
        innerView.lvComponent.isEnabled = false
        thread {
            val newStat = !item!!.enabled
            val ret = DeviceAPI.freezeComponent(pkgName, item.component?.className, !newStat)
            if (ret) {
                item.enabled = newStat
            }
            val msg = Message()
            msg.what = if (ret) 1 else 0
            msg.obj = item
            hFreeze.sendMessage(msg)
        }
    }

    private val hOnekey = object : Handler() {
        override fun handleMessage(msg: Message) {
            showApplyProfileAlert(msg.what == 1)
            innerView.btnDownloadProfile.isEnabled = true
            innerView.btnUploadProfile.isEnabled = true
            innerView.loading.visibility = View.GONE
            super.handleMessage(msg)
        }
    }

    private fun showApplyProfileAlert(hasProfile: Boolean) =
            AlertDialog.Builder(context).setTitle(R.string.alert_hint)
                    .setMessage(if (hasProfile) R.string.alert_apply_component_config else R.string.alert_no_component_config)
                    .setPositiveButton(R.string.alert_ok, null)
                    .show()


    private fun threadGetOnekeyAndApply() {
        innerView.btnDownloadProfile.isEnabled = false
        innerView.btnUploadProfile.isEnabled = false
        innerView.loading.visibility = View.VISIBLE
        thread {
            var hasProfile = false
            val ok = API.getOnekey(pkgName, versionCode)
            if (ok != null) {
                if (ok.disabledComponents != null && ok.disabledComponents!!.size != 0) {
                    hasProfile = true
                }
                if (hasProfile) {
                    DeviceAPI.freezeComponents(pkgName, ok.disabledComponents, true)
                }
            }
            hOnekey.sendEmptyMessage(if (hasProfile) 1 else 0)
        }
    }

    private val hPutOnekey = object : Handler() {
        override fun handleMessage(msg: Message) {
            // handle put onekey
            showUploadProfileAlert(msg.what == 1)
            innerView.btnDownloadProfile.isEnabled = true
            innerView.btnUploadProfile.isEnabled = true
            innerView.loading.visibility = View.GONE
            super.handleMessage(msg)
        }
    }

    private fun showUploadProfileAlert(succ: Boolean) {
        AlertDialog.Builder(context).setTitle(R.string.alert_hint)
                .setMessage(if (succ) R.string.alert_upload_ok else R.string.alert_upload_failed)
                .setPositiveButton(R.string.alert_ok, null)
                .show()
    }

    private fun threadPutOnekeyConfig() {
        // thread put onekey config
        innerView.btnDownloadProfile.isEnabled = false
        innerView.btnUploadProfile.isEnabled = false
        innerView.loading.visibility = View.VISIBLE
        thread {
            var ret = false
            try {
                val info = context.packageManager.getApplicationInfo(pkgName, 0)
                val ppu = PackageParserUtils()
                val obj = ppu.parsePackage(info.publicSourceDir, 0)
                val lstActivity = ComponentUtils.getActivityList(context, obj)
                val lstService = ComponentUtils.getServiceList(context, obj)
                val lstReceiver = ComponentUtils.getReceiverList(context, obj)
                val lstProvider = ComponentUtils.getProviderList(context, obj)
                val lstDisabled = arrayListOf<String?>()
                lstActivity.filterNot { it.enabled }.mapTo(lstDisabled) { it.component?.className }
                lstService.filterNot { it.enabled }.mapTo(lstDisabled) { it.component?.className }
                lstReceiver.filterNot { it.enabled }.mapTo(lstDisabled) { it.component?.className }
                lstProvider.filterNot { it.enabled }.mapTo(lstDisabled) { it.component?.className }
                if (lstDisabled.size != 0) {
                    ret = API.uploadOnekey(pkgName, versionCode, lstDisabled)
                }
            } catch (e: Exception) {

            }
            hPutOnekey.sendEmptyMessage(if (ret) 1 else 0)
        }
    }
}
