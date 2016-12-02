package com.rarnu.tools.neo.fragment

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.ListView
import android.widget.SearchView
import android.widget.Toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.activity.BuildPropEditActivity
import com.rarnu.tools.neo.adapter.BuildPropAdapter
import com.rarnu.tools.neo.base.BaseFragment
import com.rarnu.tools.neo.comp.LoadingView
import com.rarnu.tools.neo.data.BuildPropInfo
import com.rarnu.tools.neo.loader.BuildPropLoader
import com.rarnu.tools.neo.utils.BuildPropUtils
import kotlin.concurrent.thread

class FakeDeviceFragment : BaseFragment(), SearchView.OnQueryTextListener, AdapterView.OnItemClickListener {

    private var lvProp: ListView? = null
    private var list: MutableList<BuildPropInfo>? = null
    private var adapter: BuildPropAdapter? = null
    private var loader: BuildPropLoader? = null
    private var sv: SearchView? = null
    private var miSave: MenuItem? = null
    private var miSearch: MenuItem? = null
    private var loading: LoadingView? = null

    override fun getBarTitle(): Int = R.string.fake_device_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        lvProp = innerView?.findViewById(R.id.lvProp) as ListView?
        list = arrayListOf<BuildPropInfo>()
        adapter = BuildPropAdapter(context, list)
        lvProp?.adapter = adapter
        loader = BuildPropLoader(context)
        loading = innerView?.findViewById(R.id.loading) as LoadingView?
    }

    override fun initEvents() {
        lvProp?.onItemClickListener = this
        loader?.registerListener(0, { loader, data ->
            list?.clear()
            if (data != null) {
                list?.addAll(data)
            }
            adapter?.setNewList(list)
            loading?.visibility = View.GONE
        })
    }

    override fun initLogic() {
        loading?.visibility = View.VISIBLE
        loader?.startLoading()
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_fakedev

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) {
        miSearch = menu?.add(0, 1, 1, R.string.ab_search)
        miSearch?.setIcon(android.R.drawable.ic_menu_search)
        miSearch?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)

        sv = SearchView(context)
        sv?.setOnQueryTextListener(this)
        miSearch?.actionView = sv

        miSave = menu?.add(0, 2, 2, R.string.ab_save)
        miSave?.setIcon(android.R.drawable.ic_menu_save)
        miSave?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            2 -> threadSaveBuildProp()
        }
        return true
    }

    private val hSaving = object : Handler() {
        override fun handleMessage(msg: Message) {
            Toast.makeText(context, if (msg.what == 0) R.string.toast_buildprop_saved else R.string.toast_buildprop_save_failed, Toast.LENGTH_SHORT).show()
            loading?.visibility = View.GONE
            super.handleMessage(msg)
        }
    }

    private fun threadSaveBuildProp() {
        loading?.visibility = View.VISIBLE
        thread {
            val ret = BuildPropUtils.setBuildProp(context, list)
            hSaving.sendEmptyMessage(if (ret) 0 else 1)
        }
    }

    override fun onGetNewArguments(bn: Bundle?) {

    }

    override fun getFragmentState(): Bundle? = null

    override fun onItemClick(parent: AdapterView<*>, view: View, position: Int, id: Long) {
        val item = lvProp?.getItemAtPosition(position) as BuildPropInfo
        val inEdit = Intent(activity, BuildPropEditActivity::class.java)
        inEdit.putExtra("item", item)
        inEdit.putExtra("position", list?.indexOf(item))
        startActivityForResult(inEdit, 0)
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (resultCode != Activity.RESULT_OK) {
            return
        }
        when (requestCode) {
            0 -> {
                val item = data!!.getSerializableExtra("item") as BuildPropInfo
                val position = data.getIntExtra("position", -1)
                if (position != -1) {
                    list!![position] = item
                    adapter?.setNewList(list)
                    adapter?.filter(sv?.query.toString())
                }
            }
        }
    }

    override fun onQueryTextSubmit(query: String): Boolean = false

    override fun onQueryTextChange(newText: String): Boolean {
        adapter?.filter(newText)
        return true
    }
}
