package com.rarnu.tools.neo.fragment

import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.MotionEvent
import android.view.View
import android.widget.ImageView
import android.widget.TextView
import android.widget.Toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.activity.ChangeLogActivity
import com.rarnu.tools.neo.activity.ThanksActivity
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.base.BaseFragment
import com.rarnu.tools.neo.utils.FileUtils
import com.rarnu.tools.neo.xposed.XpStatus
import java.io.IOException
import java.util.*


class AboutFragment : BaseFragment(), View.OnClickListener, View.OnTouchListener {

    private var tvVersion: TextView? = null
    private var tvProj: TextView? = null
    private var tvIntro: TextView? = null
    private var miThanks: MenuItem? = null
    private var tvChangeLog: TextView? = null
    private var ivLogo: ImageView? = null

    override fun getBarTitle(): Int = R.string.about_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        tvVersion = innerView?.findViewById(R.id.tvVersion) as TextView?
        tvProj = innerView?.findViewById(R.id.tvProj) as TextView?
        tvIntro = innerView?.findViewById(R.id.tvIntro) as TextView?
        tvChangeLog = innerView?.findViewById(R.id.tvChangeLog) as TextView?
        ivLogo = innerView?.findViewById(R.id.ivLogo) as ImageView?
    }

    override fun initEvents() {
        tvProj?.setOnClickListener(this)
        tvChangeLog?.setOnClickListener(this)
        ivLogo?.setOnTouchListener(this)
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

    var touchCount = 0
    var lastTime = 0L

    override fun onTouch(v: View?, event: MotionEvent): Boolean {
        if (event.action == MotionEvent.ACTION_DOWN) {
            if (lastTime == 0L) {
                lastTime = System.currentTimeMillis()
                touchCount = 1
                return true
            } else {
                val t = System.currentTimeMillis()
                if (t - lastTime > 1000) {
                    touchCount = 0
                    lastTime = 0
                    return true
                } else {
                    lastTime = t
                    touchCount++
                    if (touchCount == 5) {
                        touchCount = 0
                        lastTime = 0
                        showThemeCrack()
                        return true
                    }
                }
            }
        }
        return false
    }

    private fun openUrl(resId: Int) {
        val u = Uri.parse(getString(resId))
        val inWeb = Intent(Intent.ACTION_VIEW)
        inWeb.data = u
        startActivity(inWeb)
    }

    private fun showThemeCrack() {
        val pref = context.getSharedPreferences(XpStatus.PREF, if (Build.VERSION.SDK_INT < 24) 1 else 0)
        pref.edit().putBoolean(XpStatus.KEY_SHOW_THEME_CRACK, true).apply()
        DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context.packageName)
        Toast.makeText(context, R.string.toast_hidden_function_open, Toast.LENGTH_SHORT).show()
    }
}
