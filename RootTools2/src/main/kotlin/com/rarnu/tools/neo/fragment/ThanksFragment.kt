package com.rarnu.tools.neo.fragment

import android.os.Bundle
import android.view.Menu
import com.rarnu.base.app.BaseFragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.ThanksAdapter
import com.rarnu.tools.neo.data.ThanksInfo
import com.rarnu.tools.neo.loader.ThanksLoader
import kotlinx.android.synthetic.main.fragment_thanks.view.*

/**
 * Created by rarnu on 12/5/16.
 */
class ThanksFragment : BaseFragment() {

    private var list: MutableList<ThanksInfo?>? = null
    private var adapter: ThanksAdapter? = null
    private var loader: ThanksLoader? = null

    override fun getBarTitle(): Int = R.string.thanks_name

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        list = arrayListOf()
        adapter = ThanksAdapter(context, list)
        innerView.lvThanks.adapter = adapter
        loader = ThanksLoader(context)
    }

    override fun initEvents() {
        loader?.registerListener(0, { _, data ->
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

    override fun initMenu(menu: Menu) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}