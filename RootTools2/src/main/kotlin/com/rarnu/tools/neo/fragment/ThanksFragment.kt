package com.rarnu.tools.neo.fragment

import android.os.Bundle
import android.view.Menu
import android.widget.ListView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.ThanksAdapter
import com.rarnu.tools.neo.base.BaseFragment
import com.rarnu.tools.neo.data.ThanksInfo
import com.rarnu.tools.neo.loader.ThanksLoader

/**
 * Created by rarnu on 12/5/16.
 */
class ThanksFragment : BaseFragment() {

    private var lvThanks: ListView? = null
    private var list: MutableList<ThanksInfo?>? = null
    private var adapter: ThanksAdapter? = null
    private var loader: ThanksLoader? = null

    override fun getBarTitle(): Int = R.string.thanks_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        lvThanks = innerView?.findViewById(R.id.lvThanks) as ListView?
        list = arrayListOf()
        adapter = ThanksAdapter(context, list)
        lvThanks?.adapter = adapter
        loader = ThanksLoader(context)
    }

    override fun initEvents() {
        loader?.registerListener(0, { loader, data ->
            list?.clear()
            if (data != null) {
                list?.addAll(data)
                adapter?.setNewList(list)
            }
        })
    }

    override fun initLogic() {
        loader?.startLoading()
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_thanks

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}