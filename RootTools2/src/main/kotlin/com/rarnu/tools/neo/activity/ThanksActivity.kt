package com.rarnu.tools.neo.activity

import android.os.Bundle
import com.rarnu.kt.android.BackActivity
import com.rarnu.kt.android.resStr
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.adapter.ThanksAdapter
import com.rarnu.tools.neo.data.ThanksInfo
import com.rarnu.tools.neo.loader.ThanksLoader
import kotlinx.android.synthetic.main.fragment_thanks.*

/**
 * Created by rarnu on 12/5/16.
 */
class ThanksActivity : BackActivity() {

    private var list = mutableListOf<ThanksInfo>()
    private lateinit var adapter: ThanksAdapter
    private lateinit var loader: ThanksLoader

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_thanks)
        actionBar?.title = resStr(R.string.thanks_name)

        adapter = ThanksAdapter(this, list)
        lvThanks.adapter = adapter
        loader = ThanksLoader(this)

        loader.registerListener(0) { _, data ->
            list.clear()
            if (data != null) {
                list.addAll(data)
                adapter.setNewList(list)
            }
        }

        loader.startLoading()

    }
}