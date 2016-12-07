package com.rarnu.tools.neo.fragment

import android.content.Intent
import android.content.pm.PackageInfo
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Bundle
import android.text.Html
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.TextView
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.activity.ChangeLogActivity
import com.rarnu.tools.neo.activity.ThanksActivity
import com.rarnu.tools.neo.base.BaseFragment
import com.rarnu.tools.neo.utils.FileUtils

import java.io.IOException
import java.util.Locale

class AboutFragment : BaseFragment(), View.OnClickListener {

    private var tvVersion: TextView? = null
    private var tvProj: TextView? = null
    private var tvIntro: TextView? = null
    private var miThanks: MenuItem? = null
    private var tvChangeLog: TextView? = null

    override fun getBarTitle(): Int = R.string.about_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        tvVersion = innerView?.findViewById(R.id.tvVersion) as TextView?
        tvProj = innerView?.findViewById(R.id.tvProj) as TextView?
        tvIntro = innerView?.findViewById(R.id.tvIntro) as TextView?
        tvChangeLog = innerView?.findViewById(R.id.tvChangeLog) as TextView?
    }

    override fun initEvents() {
        tvProj?.setOnClickListener(this)
        tvChangeLog?.setOnClickListener(this)
    }

    override fun initLogic() {
        var ver = "unknown"
        try {
            val info = context.packageManager.getPackageInfo(context.packageName, 0)
            ver = info.versionName
        } catch (e: Exception) {

        }

        tvVersion?.text = getString(R.string.view_about_version, ver)
        try {
            val lng = Locale.getDefault().language
            val intro = FileUtils.readAssetFile(context, if (lng == "zh") "intro_zh" else "intro")
            tvIntro?.text = intro
        } catch (e: IOException) {

        }

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_about

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) {
        menu?.clear()
        miThanks = menu?.add(0, 1, 1, R.string.ab_thanks)
        miThanks?.setIcon(android.R.drawable.ic_menu_info_details)
        miThanks?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            1 -> {
                startActivity(Intent(context, ThanksActivity::class.java))
            }
        }
        return true
    }

    override fun onGetNewArguments(bn: Bundle?) {
    }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View) {
        when (v.id) {
            R.id.tvProj -> openUrl(R.string.view_about_project_github_url)
            R.id.tvChangeLog -> startActivity(Intent(context, ChangeLogActivity::class.java))
        }
    }

    private fun openUrl(resId: Int) {
        val u = Uri.parse(getString(resId))
        val inWeb = Intent(Intent.ACTION_VIEW)
        inWeb.data = u
        startActivity(inWeb)
    }
}
