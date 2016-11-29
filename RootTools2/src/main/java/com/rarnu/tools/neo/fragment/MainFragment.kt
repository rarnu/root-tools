package com.rarnu.tools.neo.fragment

import android.app.AlertDialog
import android.content.DialogInterface
import android.content.Intent
import android.content.SharedPreferences
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.preference.Preference
import android.preference.PreferenceCategory
import android.view.Menu
import android.view.MenuItem
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.Toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.activity.*
import com.rarnu.tools.neo.api.API
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.base.BasePreferenceFragment
import com.rarnu.tools.neo.comp.PreferenceEx
import com.rarnu.tools.neo.data.UpdateInfo
import com.rarnu.tools.neo.utils.AppUtils
import com.rarnu.tools.neo.utils.HostsUtils
import com.rarnu.tools.neo.utils.UIUtils
import com.rarnu.tools.neo.xposed.XpStatus

class MainFragment : BasePreferenceFragment(), Preference.OnPreferenceClickListener, UpdateInfo.UpdateInfoReadyCallback {

    // categories
    private var catMain: PreferenceCategory? = null
    private var catMiui: PreferenceCategory? = null
    private var catAbout: PreferenceCategory? = null

    // system
    private var pFreeze: PreferenceEx? = null
    private var pComponent: PreferenceEx? = null
    private var pCleanArt: PreferenceEx? = null
    private var pCoreCrack: PreferenceEx? = null
    private var pFakeDevice: PreferenceEx? = null
    private var pMemory: PreferenceEx? = null
    // miui
    private var pTheme: PreferenceEx? = null
    private var pRemoveAd: PreferenceEx? = null
    private var pRemoveSearch: PreferenceEx? = null
    private var pMinusScreen: PreferenceEx? = null
    private var pKeepMtz: PreferenceEx? = null
    private var pRoot25: PreferenceEx? = null
    private var pNoUpdate: PreferenceEx? = null
    // about
    private var pFeedback: PreferenceEx? = null
    private var pAbout: PreferenceEx? = null


    // pref
    private var pref: SharedPreferences? = null
    private var editor: SharedPreferences.Editor? = null

    //
    private var miShare: MenuItem? = null
    private var miSettings: MenuItem? = null

    override fun getBarTitle(): Int = R.string.app_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {

        pref = context?.getSharedPreferences(XpStatus.PREF, if (Build.VERSION.SDK_INT < 24) 1 else 0)
        editor = pref?.edit()

        // MODE_WORLD_READABLE is removed on Android N!!!!!
        // categories
        catMain = findPreference(getString(R.string.catid_system)) as PreferenceCategory
        catMiui = findPreference(getString(R.string.catid_miui)) as PreferenceCategory
        catAbout = findPreference(getString(R.string.catid_about)) as PreferenceCategory

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
        pRemoveSearch = findPref(R.string.id_removesearch)
        pMinusScreen = findPref(R.string.id_minus_screen)
        pKeepMtz = findPref(R.string.id_keep_mtz)
        pRoot25 = findPref(R.string.id_root25)
        pNoUpdate = findPref(R.string.id_noupdate)

        // about
        pFeedback = findPref(R.string.id_feedback)
        pAbout = findPref(R.string.id_about)
    }

    private fun findPref(prefId: Int): PreferenceEx = findPreference(getString(prefId)) as PreferenceEx

    override fun initEvents() {
        // system
        pFreeze?.onPreferenceClickListener = this
        pComponent?.onPreferenceClickListener = this
        pCleanArt?.onPreferenceClickListener = this
        pCoreCrack?.onPreferenceClickListener = this
        pFakeDevice?.onPreferenceClickListener = this
        pMemory?.onPreferenceClickListener = this

        // miui
        pTheme?.onPreferenceClickListener = this
        pRemoveAd?.onPreferenceClickListener = this
        pRemoveSearch?.onPreferenceClickListener = this
        pMinusScreen?.onPreferenceClickListener = this
        pKeepMtz?.onPreferenceClickListener = this
        pRoot25?.onPreferenceClickListener = this
        pNoUpdate?.onPreferenceClickListener = this

        // about
        pFeedback?.onPreferenceClickListener = this
        pAbout?.onPreferenceClickListener = this
    }

    override fun initLogic() {
        loadSettings()
        setXposedRootStatus()
        UpdateInfo.getUpdateInfo(this)
    }

    override fun getFragmentLayoutResId(): Int = R.xml.main

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) {
        menu?.clear()
        miSettings = menu?.add(0, 2, 0, R.string.ab_settings)
        miSettings?.setIcon(android.R.drawable.ic_menu_preferences)
        miSettings?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        miShare = menu?.add(0, 1, 1, R.string.ab_help)
        miShare?.setIcon(android.R.drawable.ic_menu_help)
        miShare?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            1 -> showQQGroup()
            2 -> showActivityResult(SettingsActivity::class.java, 1)
        }
        return true
    }

    override fun onGetNewArguments(bn: Bundle?) {

    }

    override fun getFragmentState(): Bundle? {
        return null
    }

    private fun loadSettings() {
        pTheme?.status = pref!!.getBoolean(XpStatus.KEY_THEMECRACK, false)

        pRemoveSearch?.status = pref!!.getBoolean(XpStatus.KEY_REMOVESEARCHBAR, false)
        pMinusScreen?.status = pref!!.getBoolean(XpStatus.KEY_MINUS_SCREEN, false)
        pKeepMtz?.status = pref!!.getBoolean(XpStatus.KEY_KEEP_MTZ, false)
        pRoot25?.status = pref!!.getBoolean(XpStatus.KEY_ROOTCRACK, false)
        pCoreCrack?.status = pref!!.getBoolean(XpStatus.KEY_CORECRACK, false)
        pNoUpdate?.status = pref!!.getBoolean(XpStatus.KEY_NOUPDATE, false)

        if (pref!!.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
            pRemoveAd?.setShowSwitch(false)
        } else {
            pRemoveAd?.setShowSwitch(true)
            pRemoveAd?.status = pref!!.getBoolean(XpStatus.KEY_REMOVEAD, false)
        }

    }

    private fun setXposedRootStatus() {

        val isMIUI = AppUtils.isMIUI(context)

        pFeedback?.isEnabled = true
        pAbout?.isEnabled = true

        pFreeze?.isEnabled = !DeviceAPI.isRejected
        pComponent?.isEnabled = !DeviceAPI.isRejected
        pMemory?.isEnabled = !DeviceAPI.isRejected
        pCleanArt?.isEnabled = !DeviceAPI.isRejected
        pFakeDevice?.isEnabled = !DeviceAPI.isRejected
        pNoUpdate?.isEnabled = isMIUI && !DeviceAPI.isRejected

        if (Build.VERSION.SDK_INT >= 24) {
            pTheme?.isEnabled = isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected
            pRemoveSearch?.isEnabled = isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected
            pMinusScreen?.isEnabled = isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected
            pKeepMtz?.isEnabled = isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected
            pRoot25?.isEnabled = isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected
            pCoreCrack?.isEnabled = XpStatus.isEnable() && !DeviceAPI.isRejected
        } else {
            pTheme?.isEnabled = isMIUI && XpStatus.isEnable()
            pRemoveSearch?.isEnabled = isMIUI && XpStatus.isEnable()
            pMinusScreen?.isEnabled = isMIUI && XpStatus.isEnable()
            pKeepMtz?.isEnabled = isMIUI && XpStatus.isEnable()
            pRoot25?.isEnabled = isMIUI && XpStatus.isEnable()
            pCoreCrack?.isEnabled = XpStatus.isEnable()
        }
        pRemoveAd?.isEnabled = isMIUI && XpStatus.isEnable()
        if (!isMIUI) {
            preferenceScreen.removePreference(catMiui)
            catAbout?.removePreference(pFeedback)
        }
    }

    private fun showActivity(cls: Class<*>) = startActivity(Intent(context, cls))

    private fun showActivityResult(cls: Class<*>, req: Int) = startActivityForResult(Intent(context, cls), req)

    private fun threadWriteHost() {
        Thread(Runnable { HostsUtils.writeHost(context, pref!!.getBoolean(XpStatus.KEY_NOUPDATE, false), pref!!.getBoolean(XpStatus.KEY_REMOVEAD, false)) }).start()
    }

    private fun threadDeleteTmpFiles() {
        Thread(Runnable { DeviceAPI.forceDeleteFile("/data/data/com.miui.cleanmaster/shared_prefs/*") }).start()
    }

    override fun onPreferenceClick(preference: Preference): Boolean {
        val prefKey = preference.key
        val ex = preference as PreferenceEx
        if (prefKey == getString(R.string.id_freeze)) {
            showActivity(FreezeActivity::class.java)
        } else if (prefKey == getString(R.string.id_component)) {
            showActivity(ComponentActivity::class.java)
        } else if (prefKey == getString(R.string.id_cleanart)) {
            showActivity(CleanActivity::class.java)
        } else if (prefKey == getString(R.string.id_about)) {
            showActivity(AboutActivity::class.java)
        } else if (prefKey == getString(R.string.id_feedback)) {
            showActivity(FeedbackActivity::class.java)
        } else if (prefKey == getString(R.string.id_fake_device)) {
            showActivity(FakeDeviceActivity::class.java)
        } else if (prefKey == getString(R.string.id_memory)) {
            threadMemory()
        } else if (prefKey == getString(R.string.id_theme)) {
            ex.status = !ex.status
            editor!!.putBoolean(XpStatus.KEY_THEMECRACK, ex.status).apply()
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
        } else if (prefKey == getString(R.string.id_removead)) {
            if (pref!!.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
                showActivity(MIUIAppSettingActivity::class.java)
            } else {
                ex.status = !ex.status
                editor!!.putBoolean(XpStatus.KEY_REMOVEAD, ex.status).apply()
                DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
                threadWriteHost()
                threadDeleteTmpFiles()
            }
        } else if (prefKey == getString(R.string.id_removesearch)) {
            ex.status = !ex.status
            editor!!.putBoolean(XpStatus.KEY_REMOVESEARCHBAR, ex.status).apply()
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
        } else if (prefKey == getString(R.string.id_minus_screen)) {
            ex.status = !ex.status
            editor!!.putBoolean(XpStatus.KEY_MINUS_SCREEN, ex.status).apply()
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
        } else if (prefKey == getString(R.string.id_keep_mtz)) {
            ex.status = !ex.status
            editor!!.putBoolean(XpStatus.KEY_KEEP_MTZ, ex.status).apply()
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
        } else if (prefKey == getString(R.string.id_root25)) {
            ex.status = !ex.status
            editor!!.putBoolean(XpStatus.KEY_ROOTCRACK, ex.status).apply()
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
        } else if (prefKey == getString(R.string.id_corecrack)) {
            ex.status = !ex.status
            editor!!.putBoolean(XpStatus.KEY_CORECRACK, ex.status).apply()
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
        } else if (prefKey == getString(R.string.id_noupdate)) {
            ex.status = !ex.status
            editor!!.putBoolean(XpStatus.KEY_NOUPDATE, ex.status).apply()
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
            threadWriteHost()
        }
        return true
    }

    override fun onUpdateInfoReady(info: UpdateInfo) {
        if (info.isNewVersion(context)) {
            AlertDialog.Builder(context).setTitle(R.string.alert_hint)
                    .setMessage(getString(R.string.alert_update_message, info.versionName, info.versionCode, info.description))
                    .setPositiveButton(R.string.alert_update) { dialog, which -> downloadApk(info.url) }
                    .setNegativeButton(R.string.alert_cancel, null)
                    .show()
        }
    }

    private fun downloadApk(url: String) {
        val http = API.DOWNLOAD_URL + url
        val inDownload = Intent(Intent.ACTION_VIEW)
        inDownload.data = Uri.parse(http)
        startActivity(inDownload)
    }

    private fun showQQGroup() {
        // show qq group
        val ivLogo = ImageView(context)
        ivLogo.scaleType = ImageView.ScaleType.CENTER_INSIDE
        val w = (UIUtils.width!! * 0.8).toInt()
        ivLogo.layoutParams = ViewGroup.LayoutParams(w, w)
        ivLogo.setImageResource(R.drawable.qqgroup)
        AlertDialog.Builder(context).setTitle(R.string.alert_welcome)
                .setView(ivLogo).setMessage(R.string.alert_qq_group)
                .setPositiveButton(R.string.alert_ok, null)
                .show()
    }

    private val hMemory = object : Handler() {
        override fun handleMessage(msg: Message) {
            pMemory?.isEnabled = true
            Toast.makeText(context, R.string.toast_memory_cleaned, Toast.LENGTH_SHORT).show()
            super.handleMessage(msg)
        }
    }

    private fun threadMemory() {
        pMemory?.isEnabled = false
        Thread(Runnable {
            DeviceAPI.killProcess()
            if (pref!!.getBoolean(XpStatus.KEY_DEEP_CLEAN, false)) {
                DeviceAPI.forceDropCache()
            }
            hMemory.sendEmptyMessage(0)
        }).start()
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == 1) {
            if (pref!!.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
                pRemoveAd?.setShowSwitch(false)
            } else {
                pRemoveAd?.setShowSwitch(true)
                pRemoveAd?.status = pref!!.getBoolean(XpStatus.KEY_REMOVEAD, false)
            }
        }
    }
}
