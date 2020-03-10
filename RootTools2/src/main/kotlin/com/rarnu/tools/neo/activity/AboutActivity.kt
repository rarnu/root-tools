@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.annotation.SuppressLint
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.MotionEvent
import android.view.View
import com.rarnu.android.*
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.xposed.XpStatus
import kotlinx.android.synthetic.main.fragment_about.*

class AboutActivity : BackActivity(), View.OnClickListener, View.OnTouchListener {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.fragment_about)
        actionBar?.title = resStr(R.string.about_name)
        tvProj.setOnClickListener(this)
        tvChangeLog.setOnClickListener(this)
        tvUsage.setOnClickListener(this)
        ivLogo.setOnTouchListener(this)
        tvRepo1.setOnClickListener(this)
        tvRepo2.setOnClickListener(this)
        tvVersion.text = resStr(R.string.view_about_version, appVersionName)
        tvIntro.text = assetsReadText(if (RootApplication.isZh) "intro_zh" else "intro")
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        menu.add(0, 1, 1, R.string.ab_thanks).apply {
            setIcon(android.R.drawable.ic_menu_info_details)
            setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        }
        return super.onCreateOptionsMenu(menu)
    }

    override fun onClick(v: View) {
        when (v.id) {
            R.id.tvProj -> openUrl(R.string.view_about_project_github_url)
            R.id.tvRepo1 -> openUrl(R.string.view_about_project_repo1_url)
            R.id.tvRepo2 -> openUrl(R.string.view_about_project_repo2_url)
        }
    }

    private var touchCount = 0
    private var lastTime = 0L

    @SuppressLint("ClickableViewAccessibility")
    override fun onTouch(v: View, event: MotionEvent): Boolean {
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

    private fun openUrl(resId: Int) = startActivity(Intent(Intent.ACTION_VIEW).apply {
        data = Uri.parse(getString(resId))
    })

    private fun showThemeCrack() {
        val pref = getSharedPreferences(XpStatus.PREF, Context.MODE_PRIVATE)
        pref.edit().putBoolean(XpStatus.KEY_SHOW_THEME_CRACK, true).apply()
        DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, packageName)
        toast(resStr(R.string.toast_hidden_function_open))
    }

}
