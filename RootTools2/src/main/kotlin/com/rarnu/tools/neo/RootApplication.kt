package com.rarnu.tools.neo

import android.app.Application

/**
 * Created by rarnu on 12/5/16.
 */
class RootApplication : Application() {

    override fun onCreate() {
        super.onCreate()
        Thread.setDefaultUncaughtExceptionHandler(RootUncaughtException(this))
    }
}