package com.rarnu.tools.neo.activity

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.AdapterView
import android.widget.SearchView
import com.rarnu.kt.android.resStr
import com.rarnu.kt.android.showActionBack
import com.rarnu.kt.android.toast
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
class FakeDeviceActivity : Activity(), AdapterView.OnItemClickListener, SearchView.OnQueryTextListener {


    private var list = arrayListOf<BuildPropInfo>()
    private lateinit var adapter: BuildPropAdapter
    private lateinit var loader: BuildPropLoader
    private lateinit var sv: SearchView
    private lateinit var miSave: MenuItem
    private lateinit var miSearch: MenuItem

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_fakedev)
        actionBar?.title = resStr(R.string.fake_device_name)
        showActionBack()

        adapter = BuildPropAdapter(this, list)
        lvProp.adapter = adapter
        loader = BuildPropLoader(this)

        lvProp.onItemClickListener = this
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

    override fun onItemClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        val item = lvProp.getItemAtPosition(position) as BuildPropInfo
        val inEdit = Intent(this, BuildPropEditActivity::class.java)
        inEdit.putExtra("item", item)
        inEdit.putExtra("position", list.indexOf(item))
        startActivityForResult(inEdit, 0)
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        menu.clear()
        miSearch = menu.add(0, 1, 1, R.string.ab_search)
        miSearch.setIcon(android.R.drawable.ic_menu_search)
        miSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        sv = SearchView(this)
        sv.setOnQueryTextListener(this)
        miSearch.actionView = sv
        miSave = menu.add(0, 2, 2, R.string.ab_save)
        miSave.setIcon(android.R.drawable.ic_menu_save)
        miSave.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        return super.onCreateOptionsMenu(menu)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            android.R.id.home -> finish()
            2 -> threadSaveBuildProp()
        }
        return true
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
            runOnUiThread {
                toast(resStr(if (ret) R.string.toast_buildprop_saved else R.string.toast_buildprop_save_failed))
                loading.visibility = View.GONE
            }
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (resultCode != RESULT_OK) {
            return
        }
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
