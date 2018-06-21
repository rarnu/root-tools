package com.rarnu.tools.neo

import android.app.Application
import java.util.*

/**
 * Created by rarnu on 12/5/16.
 */
class RootApplication : Application() {

    companion object {
        var isZh = false
    }

    override fun onCreate() {
        super.onCreate()
        isZh = Locale.getDefault().language == "zh"
    }
}