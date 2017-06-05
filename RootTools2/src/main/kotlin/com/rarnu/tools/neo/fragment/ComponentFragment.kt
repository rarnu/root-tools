package com.rarnu.tools.neo.fragment

import android.content.Intent
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import com.rarnu.base.app.BaseFragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.activity.ComponentDetailActivity
import com.rarnu.tools.neo.adapter.AppAdapter
import com.rarnu.tools.neo.data.AppInfo
import com.rarnu.tools.neo.loader.AllAppLoader
import kotlinx.android.synthetic.main.fragment_component.view.*

class ComponentFragment : BaseFragment(), AdapterView.OnItemClickListener, SearchView.OnQueryTextListener {

    private var list: MutableList<AppInfo>? = null
    private var adapter: AppAdapter? = null
    private var loader: AllAppLoader? = null
    private var sv: SearchView? = null
    private var miSearch: MenuItem? = null

    override fun onItemClick(parent: AdapterView<*>, view: View, position: Int, id: Long) {
        val item = adapter?.getItem(position) as AppInfo?
        val inDetail = Intent(context, ComponentDetailActivity::class.java)
        inDetail.putExtra("pkg", item?.packageName)
        inDetail.putExtra("versionCode", item?.versionCode)
        inDetail.putExtra("name", item?.name)
        startActivity(inDetail)
    }

    override fun onQueryTextSubmit(query: String): Boolean = false

    override fun onQueryTextChange(newText: String): Boolean {
        adapter?.filter(newText)
        return true
    }

    override fun getBarTitle(): Int = R.string.component_name

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        list = arrayListOf<AppInfo>()
        adapter = AppAdapter(context, list)
        adapter?.setShowSwitch(false)
        innerView.lvApp.adapter = adapter
        loader = AllAppLoader(context)
    }

    override fun initEvents() {
        innerView.lvApp.onItemClickListener = this

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

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_component

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

    override fun onGetNewArguments(bn: Bundle?) {}

    override fun getFragmentState(): Bundle? = null
}
