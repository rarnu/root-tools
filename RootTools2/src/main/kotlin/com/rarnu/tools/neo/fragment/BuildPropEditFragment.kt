package com.rarnu.tools.neo.fragment

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.EditText
import android.widget.ImageView
import android.widget.TextView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.base.BaseDialogFragment
import com.rarnu.tools.neo.data.BuildPropInfo

class BuildPropEditFragment : BaseDialogFragment(), View.OnClickListener {

    private var tvHead: TextView? = null
    private var btnCancel: ImageView? = null
    private var btnSave: ImageView? = null
    private var etValue: EditText? = null

    override fun getBarTitle(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        tvHead = innerView?.findViewById(R.id.tvHead) as TextView?
        btnSave = innerView?.findViewById(R.id.btnSave) as ImageView?
        btnCancel = innerView?.findViewById(R.id.btnCancel) as ImageView?
        etValue = innerView?.findViewById(R.id.etValue) as EditText?
    }

    override fun initEvents() {
        btnCancel?.setOnClickListener(this)
        btnSave?.setOnClickListener(this)
    }

    override fun initLogic() {
        val item = activity.intent.getSerializableExtra("item") as BuildPropInfo
        tvHead?.text = item.buildName
        etValue?.setText(item.buildValue)
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_fakedev_edit

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View) {
        when (v.id) {
            R.id.btnSave -> {
                val info = BuildPropInfo(tvHead?.text.toString(), etValue?.text.toString())
                val inRet = Intent()
                inRet.putExtra("item", info)
                inRet.putExtra("position", activity.intent.getIntExtra("position", -1))
                activity.setResult(Activity.RESULT_OK, inRet)
                activity.finish()
            }
            R.id.btnCancel -> activity.finish()
        }
    }
}
