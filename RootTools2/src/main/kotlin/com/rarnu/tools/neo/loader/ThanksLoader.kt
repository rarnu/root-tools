package com.rarnu.tools.neo.loader

import android.content.Context
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.base.BaseLoader
import com.rarnu.tools.neo.data.ThanksInfo

/**
 * Created by rarnu on 12/5/16.
 */
class ThanksLoader(context: Context?) : BaseLoader<ThanksInfo?>(context) {
    override fun loadInBackground(): MutableList<ThanksInfo?>? = API.getThanksInfo()

}