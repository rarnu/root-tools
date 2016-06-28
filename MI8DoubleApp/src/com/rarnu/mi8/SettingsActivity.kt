package com.rarnu.mi8

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.CheckBox
import android.widget.CompoundButton
import android.widget.TextView

/**
 * Created by rarnu on 6/27/16.
 */
class SettingsActivity: Activity(), View.OnClickListener, CompoundButton.OnCheckedChangeListener {

    private var _tvTitle: TextView? = null
    private var _chkWeiXin: CheckBox? = null
    private var _chkQQ: CheckBox? = null
    private var _btnHelp: Button? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_settings)
        _tvTitle = findViewById(R.id.tvTitle) as TextView
        _chkWeiXin = findViewById(R.id.chkWeiXin) as CheckBox
        _chkQQ = findViewById(R.id.chkQQ) as CheckBox
        _btnHelp = findViewById(R.id.btnHelp) as Button

        _chkWeiXin?.isChecked = Config.isDoubleWeiXin(this)
        _chkQQ?.isChecked = Config.isDoubleQQ(this)

        _tvTitle?.setOnClickListener(this)
        _chkWeiXin?.setOnCheckedChangeListener(this)
        _chkQQ?.setOnCheckedChangeListener(this)
        _btnHelp?.setOnClickListener(this)

    }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.tvTitle -> finish()
            R.id.btnHelp -> startActivity(Intent(this, AboutActivity::class.java))
        }
    }

    override fun onCheckedChanged(buttonView: CompoundButton?, isChecked: Boolean) {
        when (buttonView!!.id) {
            R.id.chkWeiXin -> Config.setDoubleWeiXin(this, isChecked)
            R.id.chkQQ -> Config.setDoubleQQ(this, isChecked)
        }
    }
}