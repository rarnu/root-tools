package com.rarnu.mi8

import android.app.Activity
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.pm.PackageManager
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.ImageButton
import android.widget.ListView
import android.widget.Toast

class MainActivity : Activity(), View.OnClickListener {

    companion object {
        val ACTION_START_SERVICE = "com.rarnu.mi8.start"
        val ACTION_BUTTON_TITLE = "com.rarnu.mi8.button.title"
    }

    private var _btnSettings: ImageButton? = null
    private var _lvUsers: ListView? = null
    private var _btnToggleService: Button? = null
    private var _btnSyncData: Button? = null

    private var _idList: MutableList<String>? = null
    private var _pathList: MutableList<String>? = null
    private var _adapter: UserAdapter? = null

    private var _filter = IntentFilter(ACTION_BUTTON_TITLE)
    private var _receiver = ObserverStatusReceiver()

    public override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        _btnSettings = findViewById(R.id.btnSettings) as ImageButton
        _lvUsers = findViewById(R.id.lvUsers) as ListView
        _btnToggleService = findViewById(R.id.btnToggleService) as Button
        _btnSyncData = findViewById(R.id.btnSyncData) as Button

        _btnSettings?.setOnClickListener(this)
        _btnToggleService?.setOnClickListener(this)
        _btnSyncData?.setOnClickListener(this)

        _idList = UserUtils.getUserIdList()
        _pathList = UserUtils.getUsersPath(_idList!!)

        _idList?.add(0, UserUtils.zeroId)
        _pathList?.add(0, UserUtils.zeroPath)

        _adapter = UserAdapter(this, _idList, _pathList)
        _lvUsers?.adapter = _adapter

        if (checkSelfPermission(android.Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
            requestPermissions(arrayOf(android.Manifest.permission.WRITE_EXTERNAL_STORAGE), 0)
        }

        registerReceiver(_receiver, _filter)

        if (Config.isWatching(this)) {
            sendBroadcast(Intent(ACTION_START_SERVICE))
        }
        setButtonTitle()
    }

    override fun onRequestPermissionsResult(requestCode: Int, permissions: Array<out String>?, grantResults: IntArray?) {
        if (grantResults!![0] != PackageManager.PERMISSION_GRANTED) {
            Toast.makeText(this, R.string.toast_reject_permission, Toast.LENGTH_LONG).show()
        }
    }

    override fun onDestroy() {
        unregisterReceiver(_receiver)
        super.onDestroy()
    }

    private fun setButtonTitle() {
        if (ObserverService.isRunning) {
            _btnToggleService?.text = getString(R.string.btn_stop)
        } else {
            _btnToggleService?.text = getString(R.string.btn_watch)
        }
    }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnSettings -> {
                startActivityForResult(Intent(this, SettingsActivity::class.java), 0)
            }
            R.id.btnSyncData -> {
                // manual sync data
                UserUtils.syncToZero(this, _pathList!!)
                Toast.makeText(this, R.string.toast_synced, Toast.LENGTH_SHORT).show()
            }
            R.id.btnToggleService -> {
                // toggle observe service
                if (ObserverService.isRunning) {
                    Config.setWatching(this, false)
                    stopService(Intent(applicationContext, ObserverService::class.java))
                    _btnToggleService?.text = getString(R.string.btn_watch)
                } else {
                    Config.setWatching(this, true)
                    sendBroadcast(Intent(ACTION_START_SERVICE))
                    _btnToggleService?.text = getString(R.string.btn_stop)
                    Toast.makeText(this, R.string.toast_lock_screen, Toast.LENGTH_LONG).show()
                }
            }
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (Config.isWatching(this)) {
            sendBroadcast(Intent(ACTION_START_SERVICE))
        }
    }

    inner class ObserverStatusReceiver: BroadcastReceiver() {
        override fun onReceive(context: Context?, intent: Intent?) {
            setButtonTitle()
        }
    }
}
