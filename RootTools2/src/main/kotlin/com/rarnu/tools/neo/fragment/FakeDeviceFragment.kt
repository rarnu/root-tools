package com.rarnu.tools.neo.fragment

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import android.widget.Toast
import com.rarnu.base.app.BaseFragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.activity.BuildPropEditActivity
import com.rarnu.tools.neo.adapter.BuildPropAdapter
import com.rarnu.tools.neo.data.BuildPropInfo
import com.rarnu.tools.neo.loader.BuildPropLoader
import com.rarnu.tools.neo.utils.BuildPropUtils
import kotlinx.android.synthetic.main.fragment_fakedev.view.*
import kotlin.concurrent.thread

class FakeDeviceFragment : BaseFragment(), SearchView.OnQueryTextListener, AdapterView.OnItemClickListener {


    private var list: MutableList<BuildPropInfo>? = null
    private var adapter: BuildPropAdapter? = null
    private var loader: BuildPropLoader? = null
    private var sv: SearchView? = null
    private var miSave: MenuItem? = null
    private var miSearch: MenuItem? = null

    override fun getBarTitle(): Int = R.string.fake_device_name

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        list = arrayListOf()
        adapter = BuildPropAdapter(context, list)
        innerView.lvProp.adapter = adapter
        loader = BuildPropLoader(context)
    }

    override fun initEvents() {
        innerView.lvProp.onItemClickListener = this
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

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_fakedev

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu) {
        miSearch = menu.add(0, 1, 1, R.string.ab_search)
        miSearch?.setIcon(android.R.drawable.ic_menu_search)
        miSearch?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)

        sv = SearchView(context)
        sv?.setOnQueryTextListener(this)
        miSearch?.actionView = sv

        miSave = menu.add(0, 2, 2, R.string.ab_save)
        miSave?.setIcon(android.R.drawable.ic_menu_save)
        miSave?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            2 -> threadSaveBuildProp()
        }
        return true
    }

    private fun threadSaveBuildProp() {
        innerView.loading.visibility = View.VISIBLE
        thread {
            val ret = BuildPropUtils.setBuildProp(context, list)
            activity.runOnUiThread {
                Toast.makeText(context, if (ret) R.string.toast_buildprop_saved else R.string.toast_buildprop_save_failed, Toast.LENGTH_SHORT).show()
                innerView.loading.visibility = View.GONE
            }
        }
    }

    override fun onGetNewArguments(bn: Bundle?) {

    }

    override fun getFragmentState(): Bundle? = null

    override fun onItemClick(parent: AdapterView<*>, view: View, position: Int, id: Long) {
        val item = innerView.lvProp.getItemAtPosition(position) as BuildPropInfo
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
