package com.rarnu.mi8

import android.app.Activity
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.text.Html
import android.view.View
import android.widget.TextView

/**
 * Created by rarnu on 6/28/16.
 */
class AboutActivity: Activity(), View.OnClickListener {

    companion object {
        private val GITHUB = "https://github.com/rarnu/root-tools/tree/master/MI8DoubleApp"
    }

    private var _tvVersion: TextView? = null
    private var _tvAboutData: TextView? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_about)

        _tvVersion = findViewById(R.id.tvVersion) as TextView
        _tvAboutData = findViewById(R.id.tvAboutData) as TextView

        try {
            val pkg = packageManager.getPackageInfo(packageName, 0)
            _tvVersion?.text = "Ver ${pkg.versionName}"
        } catch(e: Exception) {

        }
        _tvAboutData?.text = Html.fromHtml("Rarnu's Github: <a href='$GITHUB'>Here</a>")
        _tvAboutData?.setOnClickListener(this)
    }

    override fun onClick(v: View?) {
        val inWeb = Intent(Intent.ACTION_VIEW)
        inWeb.data = Uri.parse(GITHUB)
        startActivity(inWeb)
    }

}