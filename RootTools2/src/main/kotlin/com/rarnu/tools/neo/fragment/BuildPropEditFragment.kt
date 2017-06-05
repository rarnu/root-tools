package com.rarnu.tools.neo.fragment

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.Menu
import android.view.View
import com.rarnu.base.app.BaseDialogFragment
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.data.BuildPropInfo
import kotlinx.android.synthetic.main.fragment_fakedev_edit.view.*

class BuildPropEditFragment : BaseDialogFragment(), View.OnClickListener {

    override fun getBarTitle(): Int = 0

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() { }

    override fun initEvents() {
        innerView.btnCancel.setOnClickListener(this)
        innerView.btnSave.setOnClickListener(this)
    }

    override fun initLogic() {
        val item = activity.intent.getSerializableExtra("item") as BuildPropInfo?
        innerView.tvHead.text = item?.buildName
        innerView.etValue.setText(item?.buildValue)
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_fakedev_edit

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View) {
        when (v.id) {
            R.id.btnSave -> {
                val info = BuildPropInfo(innerView.tvHead.text.toString(), innerView.etValue.text.toString())
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
