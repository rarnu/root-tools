@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.Manifest
import android.annotation.SuppressLint
import android.app.AlertDialog
import android.content.Context
import android.content.Intent
import android.content.SharedPreferences
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.preference.Preference
import android.preference.PreferenceCategory
import android.util.Log
import android.view.Menu
import android.view.MenuItem
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.Toast
import com.rarnu.kt.android.*
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.RootApplication
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.comp.PreferenceEx
import com.rarnu.tools.neo.utils.AppUtils
import com.rarnu.tools.neo.utils.HostsUtils
import com.rarnu.tools.neo.xposed.XpStatus
import kotlin.concurrent.thread

class MainActivity : PreferenceActivity(), Preference.OnPreferenceClickListener {


    private lateinit var pref: SharedPreferences

    // categories
    private lateinit var catMain: PreferenceCategory
    private lateinit var catMiui: PreferenceCategory
    private lateinit var catAbout: PreferenceCategory

    // system
    private lateinit var pFreeze: PreferenceEx
    private lateinit var pComponent: PreferenceEx
    private lateinit var pCleanArt: PreferenceEx
    private lateinit var pCoreCrack: PreferenceEx
    private lateinit var pFakeDevice: PreferenceEx
    private lateinit var pMemory: PreferenceEx

    // miui
    private lateinit var pTheme: PreferenceEx
    private lateinit var pRemoveAd: PreferenceEx
    private lateinit var pRemoveAdRoot: PreferenceEx
    private lateinit var pRemoveSearch: PreferenceEx
    private lateinit var pMinusScreen: PreferenceEx
    private lateinit var pRoot25: PreferenceEx
    private lateinit var pNoUpdate: PreferenceEx

    // about
    private lateinit var pFeedback: PreferenceEx
    private lateinit var pAbout: PreferenceEx

    // menu
    private lateinit var miShare: MenuItem
    private lateinit var miSettings: MenuItem

    override fun onCreate(savedInstanceState: Bundle?) {
        initUI()
        super.onCreate(savedInstanceState)
        actionBar?.title = resStr(R.string.app_name)

        pref = getSharedPreferences(XpStatus.PREF, Context.MODE_PRIVATE)
        XpStatus.mode = if (pref.getBoolean(XpStatus.KEY_WORK_MODE, false)) XpStatus.Mode.NDK else XpStatus.Mode.JVM
        DeviceAPI.isRejected = !DeviceAPI.mount()
        DeviceAPI.isSystemRW = DeviceAPI.isSystemRW
        val isRooted = !DeviceAPI.isRejected
        Log.e("DeviceAPI", "isRejected => " + DeviceAPI.isRejected)
        Log.e("DeviceAPI", "isSystemRW => " + DeviceAPI.isSystemRW)

        if (!DeviceAPI.isSystemRW) {
            Toast.makeText(this, R.string.toast_need_crack_system, Toast.LENGTH_SHORT).show()
        }

        val xpEnabled = XpStatus.isEnable()

        if (!xpEnabled && !isRooted) {
            Toast.makeText(this, R.string.toast_need_root_xposed, Toast.LENGTH_SHORT).show()
        } else if (!xpEnabled && isRooted) {
            Toast.makeText(this, R.string.toast_need_xposed, Toast.LENGTH_SHORT).show()
        } else if (xpEnabled && !isRooted) {
            Toast.makeText(this, R.string.toast_need_root, Toast.LENGTH_SHORT).show()
        }

        AppUtils.doScanMedia(this)
        requirePermission()
    }

    private fun requirePermission() {
        if (Build.VERSION.SDK_INT >= 23) {
            if (checkSelfPermission(Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(arrayOf(Manifest.permission.WRITE_EXTERNAL_STORAGE, Manifest.permission.READ_EXTERNAL_STORAGE), 0)
            } else {
                XpStatus.canWriteSdcard = true
            }
        } else {
            XpStatus.canWriteSdcard = true
        }
    }

    override fun onRequestPermissionsResult(requestCode: Int, permissions: Array<String>, grantResults: IntArray) {
        if (Build.VERSION.SDK_INT >= 23) {
            super.onRequestPermissionsResult(requestCode, permissions, grantResults)
            permissions.indices.forEach {
                if (permissions[it] == Manifest.permission.WRITE_EXTERNAL_STORAGE) {
                    XpStatus.canWriteSdcard = grantResults[it] == PackageManager.PERMISSION_GRANTED
                }
            }
        }
    }

    override fun getPreferenceXml() = R.xml.main

    override fun onPreparedPreference() {

        // categories
        catMain = findPref(R.string.catid_system)
        catMiui = findPref(R.string.catid_miui)
        catAbout = findPref(R.string.catid_about)

        // system
        pFreeze = findPref(R.string.id_freeze)
        pComponent = findPref(R.string.id_component)
        pCleanArt = findPref(R.string.id_cleanart)
        pCoreCrack = findPref(R.string.id_corecrack)
        pFakeDevice = findPref(R.string.id_fake_device)
        pMemory = findPref(R.string.id_memory)

        // miui
        pTheme = findPref(R.string.id_theme)
        pRemoveAd = findPref(R.string.id_removead)
        pRemoveAdRoot = findPref(R.string.id_removead_root)
        pRemoveSearch = findPref(R.string.id_removesearch)
        pMinusScreen = findPref(R.string.id_minus_screen)
        pRoot25 = findPref(R.string.id_root25)
        pNoUpdate = findPref(R.string.id_noupdate)

        // about
        pFeedback = findPref(R.string.id_feedback)
        pAbout = findPref(R.string.id_about)

        // system
        pFreeze.onPreferenceClickListener = this
        pComponent.onPreferenceClickListener = this
        pCleanArt.onPreferenceClickListener = this
        pCoreCrack.onPreferenceClickListener = this
        pFakeDevice.onPreferenceClickListener = this
        pMemory.onPreferenceClickListener = this

        // miui
        pTheme.onPreferenceClickListener = this
        pRemoveAd.onPreferenceClickListener = this
        pRemoveAdRoot.onPreferenceClickListener = this
        pRemoveSearch.onPreferenceClickListener = this
        pMinusScreen.onPreferenceClickListener = this
        pRoot25.onPreferenceClickListener = this
        pNoUpdate.onPreferenceClickListener = this

        // about
        pFeedback.onPreferenceClickListener = this
        pAbout.onPreferenceClickListener = this

        // load data
        loadSettings()
        setXposedRootStatus()
        thread {
            val info = API.getUpdateInfo()
            runOnUiThread {
                if (info != null) {
                    if (info.isNewVersion(this@MainActivity)) {
                        val str = "    " + (if (RootApplication.isZh) info.description else info.descriptionEn).replace("\\n", "\n    ")
                        alert(resStr(R.string.alert_hint),
                                resStr(R.string.alert_update_message, info.versionName, info.versionCode, str),
                                resStr(R.string.alert_update),
                                resStr(R.string.alert_cancel)) {
                            if (it == 0) {
                                downloadApk(info.url)
                            }
                        }
                    }
                }
            }
        }
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        menu.clear()
        miSettings = menu.add(0, 2, 0, R.string.ab_settings)
        miSettings.setIcon(android.R.drawable.ic_menu_preferences)
        miSettings.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        miShare = menu.add(0, 1, 1, R.string.ab_help)
        miShare.setIcon(android.R.drawable.ic_menu_help)
        miShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        return super.onCreateOptionsMenu(menu)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            1 -> showQQGroup()
            2 -> showActivityResult(SettingsActivity::class.java, 1)
        }
        return true
    }

    override fun onPreferenceClick(preference: Preference): Boolean {
        val prefKey = preference.key
        val ex = preference as PreferenceEx
        when (prefKey) {
            getString(R.string.id_freeze) -> showActivity(FreezeActivity::class.java)
            getString(R.string.id_component) -> showActivity(ComponentActivity::class.java)
            getString(R.string.id_cleanart) -> showActivity(CleanActivity::class.java)
            getString(R.string.id_about) -> showActivityResult(AboutActivity::class.java, 2)
            getString(R.string.id_feedback) -> showActivity(FeedbackActivity::class.java)
            getString(R.string.id_fake_device) -> showActivity(FakeDeviceActivity::class.java)
            getString(R.string.id_memory) -> threadMemory()
            getString(R.string.id_theme) -> {
                ex.status = !ex.status
                pref.edit().putBoolean(XpStatus.KEY_THEMECRACK, ex.status).apply()
            }
            getString(R.string.id_removead) -> {
                if (pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
                    showActivity(MIUIAppSettingActivity::class.java)
                } else {
                    ex.status = !ex.status
                    pref.edit().putBoolean(XpStatus.KEY_REMOVEAD, ex.status).apply()
                    threadDeleteTmpFiles()
                }
            }
            getString(R.string.id_removead_root) -> {
                ex.status = !ex.status
                pref.edit().putBoolean(XpStatus.KEY_REMOVEAD_ROOT, ex.status).apply()
                threadWriteHost()
                threadDeleteTmpFiles()
            }
            getString(R.string.id_removesearch) -> {
                ex.status = !ex.status
                pref.edit().putBoolean(XpStatus.KEY_REMOVESEARCHBAR, ex.status).apply()
            }
            getString(R.string.id_minus_screen) -> {
                ex.status = !ex.status
                pref.edit().putBoolean(XpStatus.KEY_MINUS_SCREEN, ex.status).apply()
            }
            getString(R.string.id_root25) -> {
                ex.status = !ex.status
                pref.edit().putBoolean(XpStatus.KEY_ROOTCRACK, ex.status).apply()
            }
            getString(R.string.id_corecrack) -> {
                ex.status = !ex.status
                pref.edit().putBoolean(XpStatus.KEY_CORECRACK, ex.status).apply()
            }
            getString(R.string.id_noupdate) -> {
                ex.status = !ex.status
                pref.edit().putBoolean(XpStatus.KEY_NOUPDATE, ex.status).apply()
            }
        }
        DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, packageName)
        return true
    }

    private fun loadSettings() {
        pTheme.status = pref.getBoolean(XpStatus.KEY_THEMECRACK, false)
        pRemoveSearch.status = pref.getBoolean(XpStatus.KEY_REMOVESEARCHBAR, false)
        pMinusScreen.status = pref.getBoolean(XpStatus.KEY_MINUS_SCREEN, false)
        pRoot25.status = pref.getBoolean(XpStatus.KEY_ROOTCRACK, false)
        pCoreCrack.status = pref.getBoolean(XpStatus.KEY_CORECRACK, false)
        pNoUpdate.status = pref.getBoolean(XpStatus.KEY_NOUPDATE, false)
        if (pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
            pRemoveAd.setShowSwitch(false)
        } else {
            pRemoveAd.setShowSwitch(true)
            pRemoveAd.status = pref.getBoolean(XpStatus.KEY_REMOVEAD, false)
        }
        pRemoveAdRoot.status = pref.getBoolean(XpStatus.KEY_REMOVEAD_ROOT, false)
    }

    private fun setXposedRootStatus() {
        val isMIUI = AppUtils.isMIUI(this)
        pFeedback.isEnabled = true
        pAbout.isEnabled = true
        // pFreeze.isEnabled = !DeviceAPI.isRejected
        // pComponent.isEnabled = !DeviceAPI.isRejected
        pMemory.isEnabled = !DeviceAPI.isRejected
        pCleanArt.isEnabled = !DeviceAPI.isRejected
        pFakeDevice.isEnabled = !DeviceAPI.isRejected
        pRemoveAdRoot.isEnabled = isMIUI && !DeviceAPI.isRejected
        pTheme.isEnabled = isMIUI && XpStatus.isEnable()
        pRemoveSearch.isEnabled = isMIUI && XpStatus.isEnable()
        pMinusScreen.isEnabled = isMIUI && XpStatus.isEnable()
        pNoUpdate.isEnabled = isMIUI && XpStatus.isEnable()
        pRoot25.isEnabled = isMIUI && XpStatus.isEnable()
        pCoreCrack.isEnabled = XpStatus.isEnable()
        pRemoveAd.isEnabled = isMIUI && XpStatus.isEnable()
        if (!isMIUI) {
            screen().removePreference(catMiui)
            catAbout.removePreference(pFeedback)
        }
        if (!pref.getBoolean(XpStatus.KEY_SHOW_THEME_CRACK, false)) {
            catMiui.removePreference(pTheme)
        }
    }

    private fun showActivity(cls: Class<*>) = startActivity(Intent(this, cls))
    private fun showActivityResult(cls: Class<*>, req: Int) = startActivityForResult(Intent(this, cls), req)
    private fun threadWriteHost() = thread { HostsUtils.writeHost(this, pref.getBoolean(XpStatus.KEY_NOUPDATE, false), pref.getBoolean(XpStatus.KEY_REMOVEAD_ROOT, false)) }

    @SuppressLint("SdCardPath")
    private fun threadDeleteTmpFiles() = thread { DeviceAPI.forceDeleteFile("/data/data/com.miui.cleanmaster/shared_prefs/*") }

    private fun downloadApk(url: String) {
        val http = API.DOWNLOAD_URL + url
        val inDownload = Intent(Intent.ACTION_VIEW)
        inDownload.data = Uri.parse(http)
        startActivity(inDownload)
    }

    private fun showQQGroup() {
        // show qq group
        val ivLogo = ImageView(this)
        ivLogo.scaleType = ImageView.ScaleType.CENTER_INSIDE
        val w = (UI.width * 0.8).toInt()
        ivLogo.layoutParams = ViewGroup.LayoutParams(w, w)
        ivLogo.setImageResource(R.drawable.qqgroup)
        AlertDialog.Builder(this).setTitle(R.string.alert_welcome)
                .setView(ivLogo).setMessage(R.string.alert_qq_group)
                .setPositiveButton(R.string.alert_ok, null)
                .show()
    }

    private fun threadMemory() {
        pMemory.isEnabled = false
        thread {
            DeviceAPI.killProcess()
            if (pref.getBoolean(XpStatus.KEY_DEEP_CLEAN, false)) {
                DeviceAPI.forceDropCache()
            }
            runOnUiThread {
                pMemory.isEnabled = true
                toast(resStr(R.string.toast_memory_cleaned))
            }
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        when (requestCode) {
            1 -> {
                if (pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
                    pRemoveAd.setShowSwitch(false)
                } else {
                    pRemoveAd.setShowSwitch(true)
                    pRemoveAd.status = pref.getBoolean(XpStatus.KEY_REMOVEAD, false)
                }
                if (!pref.getBoolean(XpStatus.KEY_SHOW_THEME_CRACK, false)) {
                    catMiui.removePreference(pTheme)
                }
            }

            2 -> {
                if (pref.getBoolean(XpStatus.KEY_SHOW_THEME_CRACK, false)) {
                    catMiui.addPreference(pTheme)
                }
            }
        }
    }
}
