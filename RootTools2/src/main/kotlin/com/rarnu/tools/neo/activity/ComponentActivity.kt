@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.content.Intent
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import com.rarnu.kt.android.BackActivity
import com.rarnu.kt.android.resStr
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.AppAdapter
import com.rarnu.tools.neo.data.AppInfo
import com.rarnu.tools.neo.loader.AllAppLoader
import kotlinx.android.synthetic.main.fragment_component.*

class ComponentActivity : BackActivity(), SearchView.OnQueryTextListener, AdapterView.OnItemClickListener {

    private var list = arrayListOf<AppInfo>()
    private lateinit var adapter: AppAdapter
    private lateinit var loader: AllAppLoader
    private lateinit var sv: SearchView
    private lateinit var miSearch: MenuItem

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_component)
        actionBar?.title = resStr(R.string.component_name)

        adapter = AppAdapter(this, list)
        adapter.setShowSwitch(false)
        lvApp.adapter = adapter
        loader = AllAppLoader(this)

        lvApp.onItemClickListener = this

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

    override fun onQueryTextSubmit(query: String?) = false

    override fun onQueryTextChange(newText: String?): Boolean {
        adapter.filter(newText)
        return true
    }

    override fun onItemClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        val item = adapter.getItem(position) as AppInfo
        val inDetail = Intent(this, ComponentDetailActivity::class.java)
        inDetail.putExtra("pkg", item.packageName)
        inDetail.putExtra("versionCode", item.versionCode)
        inDetail.putExtra("name", item.name)
        startActivity(inDetail)
    }
}
