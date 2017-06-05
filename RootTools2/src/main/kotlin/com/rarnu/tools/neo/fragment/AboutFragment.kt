package com.rarnu.tools.neo.fragment

import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.MotionEvent
import android.view.View
import android.widget.Toast
import com.rarnu.base.app.BaseFragment
import com.rarnu.base.utils.FileUtils
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.activity.ChangeLogActivity
import com.rarnu.tools.neo.activity.ManualActivity
import com.rarnu.tools.neo.activity.ThanksActivity
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.xposed.XpStatus
import kotlinx.android.synthetic.main.fragment_about.view.*

class AboutFragment : BaseFragment(), View.OnClickListener, View.OnTouchListener {

    private var miThanks: MenuItem? = null

    override fun getBarTitle(): Int = R.string.about_name

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() { }

    override fun initEvents() {
        innerView.tvProj.setOnClickListener(this)
        innerView.tvChangeLog.setOnClickListener(this)
        innerView.tvUsage.setOnClickListener(this)
        innerView.ivLogo.setOnTouchListener(this)
        innerView.tvRepo1.setOnClickListener(this)
        innerView.tvRepo2.setOnClickListener(this)
    }

    override fun initLogic() {
        var ver = "unknown"
        try {
            val info = context.packageManager.getPackageInfo(context.packageName, 0)
            ver = info.versionName
        } catch (e: Exception) {

        }
        innerView.tvVersion.text = getString(R.string.view_about_version, ver)
        try {
            val intro = FileUtils.readAssetFile(context, if (RootApplication.isZh) "intro_zh" else "intro")
            innerView.tvIntro.text = intro
        } catch (e: Exception) {

        }
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_about

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu) {
        menu.clear()
        miThanks = menu.add(0, 1, 1, R.string.ab_thanks)
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
            R.id.tvUsage -> startActivity(Intent(context, ManualActivity::class.java))
            R.id.tvRepo1 -> openUrl(R.string.view_about_project_repo1_url)
            R.id.tvRepo2 -> openUrl(R.string.view_about_project_repo2_url)
        }
    }

    var touchCount = 0
    var lastTime = 0L

    override fun onTouch(v: View?, event: MotionEvent): Boolean {
        when (event.action) {
            MotionEvent.ACTION_DOWN -> {
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
