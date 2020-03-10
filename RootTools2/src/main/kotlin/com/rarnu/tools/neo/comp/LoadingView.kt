package com.rarnu.tools.neo.comp

import android.content.Context
import android.graphics.Color
import android.util.AttributeSet
import android.view.Gravity
import android.widget.LinearLayout
import android.widget.ProgressBar
import android.widget.TextView
import com.rarnu.android.dip2px
import com.rarnu.tools.neo.R

class LoadingView : LinearLayout {

    constructor(context: Context, attrs: AttributeSet, defStyleAttr: Int) : super(context, attrs, defStyleAttr) {
        initAttrs(attrs)
    }

    constructor(context: Context, attrs: AttributeSet) : super(context, attrs) {
        initAttrs(attrs)
    }

    constructor(context: Context) : super(context) {
        initAttrs(null)
    }

    private fun initAttrs(attrs: AttributeSet?) {
        val a = context.obtainStyledAttributes(attrs, R.styleable.LoadingView, 0, 0)
        orientation = HORIZONTAL
        background = context.resources.getDrawable(R.drawable.background_layout, context.theme)
        addView(ProgressBar(context).apply {
            layoutParams = LayoutParams(36.dip2px(), 36.dip2px()).apply {
                marginStart = 12.dip2px()
                gravity = Gravity.CENTER_VERTICAL
            }
        })
        addView(TextView(context).apply {
            layoutParams = LayoutParams(LayoutParams.MATCH_PARENT, 36.dip2px()).apply {
                marginStart = 12.dip2px()
                marginEnd = 12.dip2px()
                gravity = Gravity.CENTER_VERTICAL
            }
            gravity = Gravity.CENTER_VERTICAL
            textSize = 18f
            text = a.getString(R.styleable.LoadingView_text)
            setTextColor(a.getColor(R.styleable.LoadingView_textColor, Color.BLACK))
        })
        a.recycle()
    }
}
