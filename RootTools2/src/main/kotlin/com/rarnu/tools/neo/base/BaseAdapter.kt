package com.rarnu.tools.neo.base

import android.content.Context

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseAdapter<T>(context: Context, list: MutableList<T>?) : InnerAdapter<T>(context, list)