package com.rarnu.tools.neo.activity

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import com.rarnu.kt.android.toEditable
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.data.BuildPropInfo
import kotlinx.android.synthetic.main.fragment_fakedev_edit.*

class BuildPropEditActivity : Activity(), View.OnClickListener {


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_fakedev_edit)

        btnCancel.setOnClickListener(this)
        btnSave.setOnClickListener(this)

        val item = intent.getSerializableExtra("item") as BuildPropInfo?
        tvHead.text = item?.buildName
        etValue.text = item?.buildValue?.toEditable()

    }

    override fun onClick(v: View) {
        when (v.id) {
            R.id.btnSave -> {
                val info = BuildPropInfo(tvHead.text.toString(), etValue.text.toString())
                val inRet = Intent()
                inRet.putExtra("item", info)
                inRet.putExtra("position", intent.getIntExtra("position", -1))
                setResult(Activity.RESULT_OK, inRet)
                finish()
            }
            R.id.btnCancel -> finish()
        }
    }

}
