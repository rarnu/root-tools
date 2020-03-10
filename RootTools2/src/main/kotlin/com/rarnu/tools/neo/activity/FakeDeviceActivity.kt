@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.content.Intent
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import com.rarnu.android.BackActivity
import com.rarnu.android.resStr
import com.rarnu.android.runOnMainThread
import com.rarnu.android.toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.BuildPropAdapter
import com.rarnu.tools.neo.data.BuildPropInfo
import com.rarnu.tools.neo.loader.BuildPropLoader
import com.rarnu.tools.neo.utils.BuildPropUtils
import kotlinx.android.synthetic.main.fragment_fakedev.*
import kotlin.concurrent.thread

/**
 * Created by rarnu on 9/3/16.
 */
class FakeDeviceActivity : BackActivity(), AdapterView.OnItemClickListener, SearchView.OnQueryTextListener {

    private val list = mutableListOf<BuildPropInfo>()
    private val adapter: BuildPropAdapter
    private val loader: BuildPropLoader
    private val sv: SearchView

    init {
        adapter = BuildPropAdapter(this, list)
        loader = BuildPropLoader(this).apply {
            registerListener(0) { _, data ->
                list.clear()
                if (data != null) {
                    list.addAll(data)
                }
                adapter.setNewList(list)
                loading.visibility = View.GONE
            }
        }
        sv = SearchView(this).apply {
            setOnQueryTextListener(this@FakeDeviceActivity)
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_fakedev)
        actionBar?.title = resStr(R.string.fake_device_name)
        lvProp.adapter = adapter
        lvProp.onItemClickListener = this
        loading.visibility = View.VISIBLE
        loader.startLoading()
    }

    override fun onItemClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        val item = lvProp.getItemAtPosition(position) as BuildPropInfo
        startActivityForResult(Intent(this, BuildPropEditActivity::class.java).apply {
            putExtra("item", item)
            putExtra("position", list.indexOf(item))
        }, 0)
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        menu.add(0, 1, 1, R.string.ab_search).apply {
            setIcon(android.R.drawable.ic_menu_search)
            setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
            actionView = sv
        }
        menu.add(0, 2, 2, R.string.ab_save).apply {
            setIcon(android.R.drawable.ic_menu_save)
            setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        }
        return super.onCreateOptionsMenu(menu)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            2 -> threadSaveBuildProp()
        }
        return super.onOptionsItemSelected(item)
    }

    override fun onQueryTextSubmit(query: String?) = false

    override fun onQueryTextChange(newText: String?): Boolean {
        adapter.filter(newText)
        return true
    }

    private fun threadSaveBuildProp() {
        loading.visibility = View.VISIBLE
        thread {
            val ret = BuildPropUtils.setBuildProp(this, list)
            runOnMainThread {
                toast(resStr(if (ret) R.string.toast_buildprop_saved else R.string.toast_buildprop_save_failed))
                loading.visibility = View.GONE
            }
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (resultCode != RESULT_OK) return
        when (requestCode) {
            0 -> {
                val item = data!!.getSerializableExtra("item") as BuildPropInfo
                val position = data.getIntExtra("position", -1)
                if (position != -1) {
                    list[position] = item
                    adapter.setNewList(list)
                    adapter.filter(sv.query.toString())
                }
            }
        }
    }
}
