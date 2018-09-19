package com.rarnu.tools.neo.activity

import android.app.Activity
import android.os.Bundle
import com.rarnu.kt.android.resStr
import com.rarnu.kt.android.showActionBack
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import kotlinx.android.synthetic.main.fragment_changelog.*
import kotlin.concurrent.thread

/**
 * Created by rarnu on 12/7/16.
 */
class ChangeLogActivity : Activity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_changelog)
        actionBar?.title = resStr(R.string.view_changelog)
        showActionBack()

        thread {
            val list = API.getAllUpdateInfo()
            runOnUiThread {
                var str = ""
                list?.filterNotNull()?.filter {
                    if (RootApplication.isZh) {
                        it.description.trim { it <= ' ' } != ""
                    } else {
                        it.descriptionEn.trim { it <= ' ' } != ""
                    }
                }?.forEach { str += "${it.versionName} (${it.versionCode})\n\n    ${if (RootApplication.isZh) it.description else it.descriptionEn}\n\n" }
                str = str.replace("\\n", "\n    ")
                tvChangeLog.text = str
            }
        }
    }

}