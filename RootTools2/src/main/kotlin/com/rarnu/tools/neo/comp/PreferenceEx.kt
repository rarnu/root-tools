package com.rarnu.tools.neo.comp

import android.content.Context
import android.preference.Preference
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.RelativeLayout
import android.widget.Switch
import android.widget.TextView
import com.rarnu.tools.neo.R

class PreferenceEx : Preference {

    private var layPref: RelativeLayout? = null
    private var prefIcon: ImageView? = null
    private var prefTitle: TextView? = null
    private var prefSummary: TextView? = null
    private var prefStatus: Switch? = null

    private var innerView: View? = null
    private var showSwitch = false
    private var showIcon = true
    private var isOn = false

    constructor(context: Context, attrs: AttributeSet, defStyleAttr: Int) : super(context, attrs, defStyleAttr) {
        initAttr(attrs)
    }

    constructor(context: Context, attrs: AttributeSet) : super(context, attrs) {
        initAttr(attrs)
    }

    constructor(context: Context) : super(context) {
    }

    private fun initAttr(attrs: AttributeSet?) {
        if (attrs != null) {
            val a = context.obtainStyledAttributes(attrs, R.styleable.PreferenceEx, 0, 0)
            showSwitch = a.getBoolean(R.styleable.PreferenceEx_showSwitch, false)
            showIcon = a.getBoolean(R.styleable.PreferenceEx_showIcon, true)
            a.recycle()
        }
    }

    override fun onBindView(view: View) {
        try {
            super.onBindView(view)
        } catch (e: Exception) {

        }

        prefTitle?.text = title
        prefSummary?.text = summary
        if (summary == null || summary == "") {
            prefSummary?.visibility = View.GONE
        }
        prefStatus?.isChecked = isOn
        prefIcon?.setImageDrawable(icon)
    }

    override fun onCreateView(parent: ViewGroup): View? {
        super.onCreateView(parent)
        if (innerView == null) {
            innerView = LayoutInflater.from(context).inflate(R.layout.comp_preference, parent, false)
            layPref = innerView?.findViewById(R.id.layPref) as RelativeLayout?
            prefIcon = innerView?.findViewById(R.id.prefIcon) as ImageView?
            prefTitle = innerView?.findViewById(R.id.prefTitle) as TextView?
            prefSummary = innerView?.findViewById(R.id.prefSummary) as TextView?
            prefStatus = innerView?.findViewById(R.id.prefStatus) as Switch?
            prefStatus?.visibility = if (showSwitch) View.VISIBLE else View.GONE
            prefIcon?.visibility = if (showIcon) View.VISIBLE else View.GONE
        }
        return innerView
    }

    override fun setTitle(titleResId: Int) {
        super.setTitle(titleResId)
        prefTitle?.setText(titleResId)
    }

    override fun setSummary(summaryResId: Int) {
        super.setSummary(summaryResId)
        if (summary == null || summary == "") {
            prefSummary?.visibility = View.GONE
        }
    }

    override fun setIcon(iconResId: Int) {
        super.setIcon(iconResId)
        prefIcon?.setImageDrawable(icon)
    }

    fun setShowSwitch(on: Boolean) {
        showSwitch = on
        prefStatus?.visibility = if (on) View.VISIBLE else View.GONE
    }

    fun setShowIcon(on: Boolean) {
        prefIcon?.visibility = if (on) View.VISIBLE else View.GONE
    }

    var status: Boolean
        get() = prefStatus!!.isChecked
        set(on) {
            isOn = on
            prefStatus?.isChecked = on
        }
}
