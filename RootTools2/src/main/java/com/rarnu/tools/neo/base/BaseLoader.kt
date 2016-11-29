package com.rarnu.tools.neo.base

import android.content.AsyncTaskLoader
import android.content.Context

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseLoader<T>(context: Context?) : AsyncTaskLoader<MutableList<T>?>(context) {

    abstract override fun loadInBackground(): MutableList<T>?

    override fun onStartLoading() = forceLoad()

    override fun onCanceled(data: MutableList<T>?) = super.onCanceled(data)

    override fun onStopLoading() {
        cancelLoad()
    }

    override fun onReset() = stopLoading()

}