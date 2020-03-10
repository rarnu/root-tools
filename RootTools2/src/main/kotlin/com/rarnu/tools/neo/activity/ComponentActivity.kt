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
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.AppAdapter
import com.rarnu.tools.neo.data.AppInfo
import com.rarnu.tools.neo.loader.AllAppLoader
import kotlinx.android.synthetic.main.fragment_component.*

class ComponentActivity : BackActivity(), SearchView.OnQueryTextListener, AdapterView.OnItemClickListener {

    private val list = mutableListOf<AppInfo>()
    private val adapter: AppAdapter
    private val loader: AllAppLoader

    init {
        adapter = AppAdapter(this, list).apply {
            setShowSwitch(false)
        }
        loader = AllAppLoader(this).apply {
            registerListener(0) { _, data ->
                list.clear()
                if (data != null) {
                    list.addAll(data)
                }
                adapter.setNewList(list)
                loading.visibility = View.GONE
            }
        }

    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_component)
        actionBar?.title = resStr(R.string.component_name)
        lvApp.adapter = adapter
        lvApp.onItemClickListener = this
        loading.visibility = View.VISIBLE
        loader.startLoading()
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        menu.add(0, 1, 1, R.string.ab_search).apply {
            setIcon(android.R.drawable.ic_menu_search)
            setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
            actionView = SearchView(this@ComponentActivity).apply {
                setOnQueryTextListener(this@ComponentActivity)
            }
        }
        return super.onCreateOptionsMenu(menu)
    }

    override fun onQueryTextSubmit(query: String?) = false

    override fun onQueryTextChange(newText: String?): Boolean {
        adapter.filter(newText)
        return true
    }

    override fun onItemClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        val item = adapter.getItem(position) as AppInfo
        startActivity(Intent(this, ComponentDetailActivity::class.java).apply {
            putExtra("pkg", item.packageName)
            putExtra("versionCode", item.versionCode)
            putExtra("name", item.name)
        })
    }
}
